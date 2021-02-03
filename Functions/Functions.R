# Functions that need to be roxygenized

library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)

json_convert <- function (file_name)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  # Takes a json and converts it into a dataframe
  the_data_file <- fromJSON(file_name)
  # Get the sports vu data
  moments <- the_data_file$events$moments
  
  # Function for extracting infomration from JSON
  extractbb <- function (listbb)
  {# df <- unlist(listbb,recursive = FALSE)
    df <- listbb
    # str(df)
    quarters <- unlist(lapply(df, function(x) x[[1]]))
    game_clock <- unlist(lapply(df, function(x) x[[3]]))
    shot_clock <- unlist(lapply(df, function(x) ifelse(is.null(x[[4]]), 'NA', x[[4]])))
    moment_details <- (lapply(df, function(x) x[[6]]))
    x3 <-  mapply(cbind, moment_details, game_clock, shot_clock, quarters, SIMPLIFY = F)
    x4 <- do.call('rbind', x3)
    return (x4)
  }
  
  extracted <- lapply(moments, function (x) {extractbb(x)})
  lengthmm <- the_data_file$events$eventId
  extracted <- mapply(cbind, extracted, "event.id"=lengthmm, SIMPLIFY=F)
  
  # Remove events that are NAs
  final <- (lapply(extracted, function(x) {
    if ((length(unlist(x))) <= 1) {x <- NA} 
    return(x)
  }))
  
  # Merge the file
  extracted <- do.call('rbind', final)
  extracted <- as_tibble(extracted)
  extracted[extracted == "NA" ] = NA
  all_movement <- extracted
  
  # Lets join the movement to player id
  headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter","event.id")
  colnames(all_movement) <- headers
  all_movement <- tibble(all_movement)
  
  home_players <- the_data_file$events$home$players[[1]]
  away_players <- the_data_file$events$visitor$players[[1]]
  colnames(home_players)[3] <- "player_id"
  colnames(away_players)[3] <- "player_id"
  
  # Change all to character for joining
  all_movement$player_id <- as.character(all_movement$player_id)
  home_players$player_id <- as.character(home_players$player_id)
  away_players$player_id <- as.character(away_players$player_id)
  
  # Join dataframes of away, home players to all movement
  home_movements <- merge(all_movement, home_players, by = "player_id")
  home_movements$team_value <- "home"
  away_movements <- merge(all_movement, away_players, by = "player_id")
  away_movements$team_value <- "away"
  ball_movement <- all_movement %>% filter(player_id == -1)
  ball_movement$jersey <- NA
  ball_movement$position <- NA
  ball_movement$team_id <- NA
  ball_movement$lastname <- "ball"
  ball_movement$firstname <- NA
  
  # Bind them all together
  all_movements <- bind_rows(home_movements, away_movements, ball_movement)
  cols_numeric <- colnames(all_movements)[2:9]
  # all_movements[, 6:13] <- lapply(all_movements[, 6:13], factorconvert)
  all_movements <- all_movements %>% 
    mutate_at(cols_numeric, as.numeric) %>% 
    dplyr::arrange(quarter, desc(game_clock), x_loc, y_loc) %>%
    rename(event_id = event.id)
  return(all_movements)
}

# Function to calculate total player distance traveled
travel_dist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  # Removes big jumps - Limiting to changes of less than 1 foot per .04 seconds means 
  # anything over 17 mph will be excluded, this seems reasonable
  diff <- as_tibble(bind_cols(diffx = diffx, diffy = diffy))
  diff <- subset(diff, abs(diffx) < 1 & abs(diffy) < 1)
  ##Back to code
  diffx <- as_vector(diff$diffx)
  diffy <- as_vector(diff$diffy)
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <- sqrt(a)
  (sum(b))
}

player_dist <- function(lastnameA, lastnameB, eventID) {
  # Functions finds the distance of the player, assumes you have a dataframe all_movements with player info
  df <- all_movements[which((all_movements$lastname == lastnameA | all_movements$lastname == lastnameB) & all_movements$event_id == eventID),]
  dfA <- df %>% filter(lastname == lastnameA) %>% select(x_loc, y_loc) 
  dfB <- df %>% filter(lastname == lastnameB) %>% select(x_loc, y_loc) 
  df_l <- 1:nrow(dfA)
  distsdf <- unlist(lapply(df_l,function(x) {dist(bind_rows(dfA[x,], dfB[x,]))}))
  return(distsdf)
}

get_game_clock <- function(lastnameA,eventID){
  # Function gets the glame clock, assumes there is a dataframe all_movements with player info
  alldf <- all_movements[which((all_movements$lastname == lastnameA) & all_movements$event_id == eventID),]
  game_clock <- alldf$game_clock
  return(as_tibble(game_clock))
}

player_dist_matrix <- function(eventID) {
  # Function creates a matrix of all player/ball distances with each other
  # assumes there a dataframe all_movements with player info
  players <- all_movements %>% filter(event_id == pickeventID) %>% select(lastname) %>% distinct(lastname)
  players2 <- players
  bigdistance <- unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {
      test = player_dist(X, Y, pickeventID)
    })
  }), recursive=FALSE)
  bigdistance_names <- unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {
      paste(X, Y,sep = "_")
    })
  }), recursive=FALSE)
  bigdistancedf <- as.matrix(do.call('cbind', bigdistance))
  colnames(bigdistancedf) <- bigdistance_names
  bigdistancedf <- bigdistancedf[,colSums(bigdistancedf^2) !=0]
  bigdistancedf <- as_tibble(bigdistancedf)
  clockinfo <- get_game_clock("ball",eventID)
  bigdistancedf$game_clock <- clockinfo$game_clock
  return (bigdistancedf)
}

get_pbp <- function(gameid) {
  # Grabs the play by play data from the NBA site
  URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=", gameid, 
                "&RangeType=2&StartPeriod=1&StartRange=0", sep = "")
  the_data_file <- fromJSON(URL1)
  test <- the_data_file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the_data_file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)
}

chull_area <- function(X, Y) {
  # Calculates the convex hull area
  df_hull <- tibble(X = X, Y = Y)
  c.hull <- chull(df_hull)
  # You need five points to draw four line segments, so we add the first set of points at the end
  c.hull <- c(c.hull, c.hull[1])
  chull.coords <- df_hull[c.hull ,]
  chull.poly <- Polygon(chull.coords, hole=F)
  chull.area <- chull.poly@area
  return (chull.area)
  }

chull_areabyteam <- function (total, balltime) {
  # Function returns a dataframe with event id and convex hull area for each team
  # Function requires an input of a dataframe with the rotated plays and a dataframe indicating event/time
  # for calculating convex hull area
  allsum <- NULL
  teams <- as.list((unique(total$team_id)))
  teams <- teams[!is.na(teams)]
  for(i in 1:(nrow(balltime)))
    {
      temp <- total %>% filter(event_id == balltime$event_id[i] & game_clock == balltime$clock28[i]) %>% 
        filter(lastname!="ball")
        if (nrow(temp) == 10) {
          dfall <- lapply(teams, function (x) { df <- temp %>% filter(team_id == x)
        if (nrow(df) == 5) {area <- (chull_area(df$x_loc_r,df$y_loc_r))
          area
          }
    })
    df <- cbind(balltime$event_id[i], teams[[1]], dfall[[1]], teams[[2]], dfall[[2]])  
    allsum <- bind_rows(df, allsum)
    }
  }
  allsum <- as_tibble(allsum)
  colnames(allsum) <- c("event_id", "team1", "team1_area", "team2", "team2_area")
  return(allsum)
}

player_position <- function(eventID, gameClock, df) {
  # Returns positions of all players at a time
  # Requires data in total and balltime
    dfall <- df %>% 
      filter(game_clock == gameClock, event_id == eventID) %>%
      filter(lastname != "ball") %>%
      mutate(name = paste(firstname, lastname)) %>%
      select(team_id, x_loc, y_loc, name)
    colnames(dfall) <- c('ID','X','Y','NAME')
    return(dfall)
  }

chull_plot <- function(eventID, gameClock, df) {
  # Returns a data frame with the coordinates of a convex hull
  # Requires player_position for info
    df2 <- player_position(eventID, gameClock, df)
    df_hull2 <- df2 %>% 
      dplyr::filter(ID == min(ID)) %>% 
      select(X, Y)
    df_hull3 <- df2 %>%
      dplyr::filter(ID == max(ID)) %>% 
      select(X, Y)
    c.hull2 <- chull(df_hull2)
    c.hull3 <- chull(df_hull3)
    #You need five points to draw four line segments, so we add the fist set of points at the end
    c.hull2 <- c(c.hull2, c.hull2[1])
    c.hull3 <- c(c.hull3, c.hull3[1])
    df2 <- as.data.frame(cbind(1,df_hull2[c.hull2,]$X, df_hull2[c.hull2,]$Y))
    df3 <- as.data.frame(cbind(2,df_hull3[c.hull3,]$X, df_hull3[c.hull3,]$Y))
    dfall <- bind_rows(df2, df3)
    colnames(dfall) <- c('ID','X','Y')
    return(dfall)
}

# This one uses map and is better
chull_plot2 <- function(x, y) {
  chull1 <- chull(x, y)
  chull2 <- c(chull1, chull1[1])
  return(chull2)
}

chull_plot_centroid <- function(eventID, gameClock, df) {
  # Returns a data frame with the centroid of a convex hull
  # Requires player_position for info
      df2 <- player_position(eventID, gameClock, df)
      df_hull2 <- df2 %>% filter(ID == min(ID)) %>% select(X,Y)
      df_hull3 <- df2 %>% filter(ID == max(ID)) %>% select(X,Y)
      c.hull2 <- chull(df_hull2)
      c.hull3 <- chull(df_hull3)
      df2centroid <- c(1, mean(df_hull2[c.hull2,]$X), mean(df_hull2[c.hull2,]$Y))
      df3centroid <- c(2, mean(df_hull3[c.hull3,]$X), mean(df_hull3[c.hull3,]$Y))
      dfall <- as_tibble(rbind(df2centroid, df3centroid))
      colnames(dfall) <- c('ID','X','Y')
      return(dfall)
}

chull_plot_area <- function(eventID, gameClock) {
  # Returns a data frame with the area of each convex hull by team ID
  # Requires player_position for info
  df2 <- player_position(eventID, gameClock)
  df2area <- df2 %>% 
    group_by(ID) %>% 
    summarise(area = chull_area(X,Y)) %>%
    select (ID, area)
  return (df2area)
}

# Function to calculate player distance traveled
distance_mean <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  # Removes big jumps - Limiting to changes of less than 1 foot per .04 seconds means 
  # anything over 17 mph will be excluded, this seems reasonable
  diff <- as_tibble(bind_cols(diffx = diffx, diffy = diffy))
  diff <- subset(diff, abs(diffx) < 1 & abs(diffy) < 1)
  # Back to code
  diffx <- as_vector(diff$diffx)
  diffy <- as_vector(diff$diffy)
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <- sqrt(a)
  b <- mean(b, na.rm = T)
}

velocity_mean <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc)))
  diffy <- as_vector((diff(yloc)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <-sqrt(a)*25 #(distance in feet per second), 25 because intervals are .04 seconds
  b <- mean(b, na.rm = T)
}

acceleration_mean <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc, differences = 2)))
  diffy <- as_vector((diff(yloc, differences = 2)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <-sqrt(a)*25*25 #(distance in feet per second)
  b <- mean(b, na.rm = T)
}

jerk_mean <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc, differences = 3)))
  diffy <- as_vector((diff(yloc, differences = 3)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <- sqrt(a)*25/32.173*25*25 #(distance in feet per second)
  b <- mean(b, na.rm = T)
}

velocity <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc)))
  diffy <- as_vector((diff(yloc)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <-sqrt(a)*25 #(distance in feet per second)
  b
}

acceleration <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc, differences = 2)))
  diffy <- as_vector((diff(yloc, differences = 2)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <-sqrt(a)*25*25 #(distance in feet per second)
  b
}

jerk <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc, differences = 3)))
  diffy <- as_vector((diff(yloc, differences = 3)))
  diffx2 <- diffx^2
  diffy2 <- diffy^2
  a <- diffx2 + diffy2
  b <- sqrt(a)*25/32.173*25*25 #(distance in feet per second)
  b
}

game_clock_reformat <- function(clock) {
  clock_orig <- if(clock >= 60){
    ms(seconds_to_period((clock)))
  }else{
    lubridate::seconds(seconds_to_period(clock))
  } # Make sure clock has at least 60 seconds for ms conversion
  
  if(clock < 60) {
    clock_orig <- str_remove(clock_orig, "S") # Needs to be inclusive to account for all possible values if clock transitions
                                              # then should also change to seconds 
    clock_orig
  } else{ # Only change game clock to seconds if less than 60
  game_clock_reformat <- str_replace(clock_orig, "M ", ":")
  game_clock_reformat <- str_remove(game_clock_reformat, "S")
  game_clock_df <- as_tibble(game_clock_reformat)
  game_clock_df <- separate(game_clock_df, value, sep = ":", into = c("A", "B"))
  clock_final <- game_clock_df %>% 
    mutate(B = as.numeric(B),
         B = round(B, 2),
         B = as.character(B),
         B = ifelse(str_length(B) == 1,
                    paste0(0, B),
                    B)) %>%
    unite("game_clock_reformat", A:B, sep = ":") %>%
    as_vector()
  clock_final
  }
}



## Court Plotting
# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
#   tt <- seq(start*pi, end*pi, length.out=npoints)
#   data.frame(
#     x = center[1] + diameter / 2 * cos(tt),
#     y = center[2] + diameter / 2 * sin(tt)
#   )
# }
# 
# # Gives y coordinates of the opposite side
# rev_y <- function(y) 94-y
# 
# # From x and y coordinates for a line (represented by a polygon here),
# # a number of group and a short description
# # creates a data.frame for this line
# # in order to use it with ggplot2.
# new_coords <- function(x, y, group, descri){
#   new_coords_df <- data.frame(x = x, y = y)
#   new_coords_df$group <- group
#   new_coords_df$side <- 1
#   group <- group + 1
#   
#   # The same thing for the opposite side
#   new_coords_df2 <- data.frame(x = x, y = rev_y(y))
#   new_coords_df2$group <- group
#   new_coords_df2$side <- 2
#   group <<- group + 1
#   
#   # On reunit les donnees
#   new_coords_df <- rbind(new_coords_df, new_coords_df2)
#   new_coords_df$descri <- descri
#   
#   return(new_coords_df)
# }
# 
# # Restricted area
# cercle_np_out <- circle_fun(center = c(25,5+3/12), diameter = (4+1/6)*2)
# cercle_np_in <- circle_fun(center = c(25,5+3/12), diameter = 4*2)
# # Three point
# cercle_3pts_out <- circle_fun(center = c(25,5+3/12), diameter = (23+9/12)*2)
# cercle_3pts_in <- circle_fun(center = c(25,5+3/12), diameter = (23+7/12)*2)
# # Hoop
# cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# # Free Throws
# cercle_lf_out <- circle_fun(center = c(25,19), diameter = 6*2)
# cercle_lf_in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
# # Center Circle
# cercle_mil_out <- circle_fun(center = c(25,47), diameter = 6*2)
# cercle_mil_in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)
# # Small Center Circle
# cercle_mil_petit_out <- circle_fun(center = c(25,47), diameter = 2*2)
# cercle_mil_petit_in <- circle_fun(center = c(25,47), diameter = (2-1/6)*2)
# 
# group <- 1
# court <- new_coords(c(0-1/6,0-1/6,50 + 1/6,50 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
# court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
# court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
# court <- rbind(court, new_coords(x = c(0,0,3,3), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur gauche"))
# court <- rbind(court, new_coords(x = c(47,47,50,50), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
# court <- rbind(court, new_coords(x = c(3,3,3+1/6,3+1/6), y = c(0,14,14,0), group = group, descri = "3pts bas gauche"))
# court <- rbind(court, new_coords(x = c(47-1/6,47-1/6,47,47), y = c(0,14,14,0), group = group, descri = "3pts bas droit"))
# court <- rbind(court, new_coords(x = c(17,17,17+1/6,17+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
# court <- rbind(court, new_coords(x = c(33-1/6,33-1/6,33,33), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
# court <- rbind(court, new_coords(x = c(17,17,33,33), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
# court <- rbind(court, new_coords(x = c(14-1/6,14-1/6,14,14), y = c(0,1/2,1/2,0), group = group, descri = "marque fond gauche"))
# court <- rbind(court, new_coords(x = c(36,36,36+1/6,36+1/6), y = c(0,1/2,1/2,0), group = group, descri = "marque fond droit"))
# court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF gauche interieur"))
# court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF droite interieur"))
# court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
# court <- rbind(court, new_coords(x = c(cercle_3pts_out[31:220,"x"], rev(cercle_3pts_in[31:220,"x"])),
#                                  y = c(cercle_3pts_out[31:220,"y"], rev(cercle_3pts_in[31:220,"y"])), group = group, descri = "cercle 3pts"))
# court <- rbind(court, new_coords(x = c(cercle_np_out[1:250,"x"], rev(cercle_np_in[1:250,"x"])),
#                                  y = c(cercle_np_out[1:250,"y"], rev(cercle_np_in[1:250,"y"])), group = group, descri = "cercle non passage en force"))
# court <- rbind(court, new_coords(x = c(20+1/6,20+1/6,20+8/12,20+8/12), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas gauche cercle LF"))
# court <- rbind(court, new_coords(x = c(30-8/12,30-8/12,30-1/6,30-1/6), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas droite cercle LF"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[1:250,"x"], rev(cercle_lf_in[1:250,"x"])),
#                                  y = c(cercle_lf_out[1:250,"y"], rev(cercle_lf_in[1:250,"y"])), group = group, descri = "cercle LF haut"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[250:269,"x"], rev(cercle_lf_in[250:269,"x"])),
#                                  y = c(cercle_lf_out[250:269,"y"], rev(cercle_lf_in[250:269,"y"])), group = group, descri = "cercle LF partie 1"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[288:308,"x"], rev(cercle_lf_in[288:308,"x"])),
#                                  y = c(cercle_lf_out[288:308,"y"], rev(cercle_lf_in[288:308,"y"])), group = group, descri = "cercle LF partie 2"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[327:346,"x"], rev(cercle_lf_in[327:346,"x"])),
#                                  y = c(cercle_lf_out[327:346,"y"], rev(cercle_lf_in[327:346,"y"])), group = group, descri = "cercle LF partie 3"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[365:385,"x"], rev(cercle_lf_in[365:385,"x"])),
#                                  y = c(cercle_lf_out[365:385,"y"], rev(cercle_lf_in[365:385,"y"])), group = group, descri = "cercle LF partie 4"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[404:423,"x"], rev(cercle_lf_in[404:423,"x"])),
#                                  y = c(cercle_lf_out[404:423,"y"], rev(cercle_lf_in[404:423,"y"])), group = group, descri = "cercle LF partie 5"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[442:462,"x"], rev(cercle_lf_in[442:462,"x"])),
#                                  y = c(cercle_lf_out[442:462,"y"], rev(cercle_lf_in[442:462,"y"])), group = group, descri = "cercle LF partie 6"))
# court <- rbind(court, new_coords(x = c(cercle_lf_out[481:500,"x"], rev(cercle_lf_in[481:500,"x"])),
#                                  y = c(cercle_lf_out[481:500,"y"], rev(cercle_lf_in[481:500,"y"])), group = group, descri = "cercle LF partie 7"))
# court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF gauche"))
# court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF gauche"))
# court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF gauche"))
# court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF gauche"))
# court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF droite"))
# court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF droite"))
# court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF droite"))
# court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF droite"))
# court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
# court <- rbind(court, new_coords(x = c(cercle_mil_out[250:500,"x"], rev(cercle_mil_in[250:500,"x"])),
#                                  y = c(cercle_mil_out[250:500,"y"], rev(cercle_mil_in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
# court <- rbind(court, new_coords(x = c(cercle_mil_petit_out[250:500,"x"], rev(cercle_mil_petit_in[250:500,"x"])),
#                                  y = c(cercle_mil_petit_out[250:500,"y"], rev(cercle_mil_petit_in[250:500,"y"])), group = group, descri = "cercle milieu petit"))
# court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))
# 
# 
# # Given the angle theta and the court data frame,
# # rotates the coordinates of the court by an angle theta
# rotate_court <- function(court, theta=pi/2){
#   court_r <- court
#   court_r$x <- court_r$x / 180 * pi
#   court_r$y <- court_r$y / 180 * pi
#   matrice_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
#   coords_r <- apply(court_r[,c("x","y")], 1, function(x) x %*% matrice_r)
#   court_r$x <- coords_r[1,] ; court_r$y <- coords_r[2,]
#   court_r$x <- court_r$x * 180 / pi
#   court_r$y <- court_r$y * 180 / pi
#   return(court_r)
# }
# 
# # Whole court with rotation
# P_180 <- ggplot() + geom_polygon(data = rotate_court(court, theta = pi/2), aes(x = x, y = y, group = group), col = "gray") +
#   coord_equal() +
#   xlim(-2, 96) +
#   ylim(-55, 2) +
#   scale_x_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
#   scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50)) +
#   xlab("") + ylab("") +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(), axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(), axis.title = element_blank(),
#         legend.position = "none"
#   )
# 
# P_half_180 <- ggplot() + geom_polygon(data = rotate_court(court[court$side==1,], theta = pi/2), aes(x = x, y = y, group = group), col = "gray") +
#   coord_equal() +
#   xlim(-2, 50) +
#   ylim(-55, 2) +
#   scale_x_continuous(breaks = c(0, 23.5, 47)) +
#   scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50)) +
#   xlab("") + ylab("") +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(), axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(), axis.title = element_blank(),
#         legend.position = c(1,1)
#   )



# Plotting a court using ballr source code

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

court_themes_half = list(
  light = list(
    court = 'white',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  )
)


plot_court = function(court_theme = court_themes_half$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }

  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )

  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))

  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))

  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))

  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)

  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")

  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)

  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")

  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")

  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)

  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )

  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )


  court_points <- court_points

  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    labs(caption = "Plot inspired by ballr by @toddwschneider") +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'white', color = 'white'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0)),
      plot.caption = element_text(size = 5),
      plot.title = element_text(hjust = 0.5)
    )
}



