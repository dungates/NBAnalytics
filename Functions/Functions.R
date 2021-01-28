###NBA_SportVU FUNCTIONS

library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)

json_convert <- function (file_name)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  # Takes a json and converts it into a dataframe
  the_data_file <- fromJSON(file_name)
  ##Get the sports vu data
  moments <- the_data_file$events$moments
  
  ## Function for extracting infomration from JSON
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
  
  ### Merge the file
  extracted <- do.call('rbind', final)
  extracted <- as.data.frame(extracted)
  extracted[extracted == "NA" ] = NA
  all_movement <- extracted
  
  ## Lets join the movement to player id
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
  away_movements <- merge(all_movement, away_players, by = "player_id")
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

## Function to calculate player distance traveled
travel_dist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  ##Removes big jumps - Limiting to changes of less than 1 foot per .04 seconds means 
  # anything over 17 mph will be excluded, this seems reasonable
  diff <- as_tibble(bind_cols(diffx = diffx, diffy = diffy))
  diff <- subset(diff, abs(diffx) < 1 & abs(diffy) < 1)
  ##Back to code
  diffx <- as_vector(diff$diffx)
  diffy <- as_vector(diff$diffy)
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a <- diffx2 + diffy2
  b <-sqrt(a)
  (sum(b))  
}

player_dist <- function(lastnameA, lastnameB, eventID) {
  #Functions finds the distance of the player, assumes you have a dataframe all_movements with player info
  df <- all_movements[which((all_movements$lastname == lastnameA | all_movements$lastname == lastnameB) & all_movements$event_id == eventID),]
  dfA <- df %>% filter(lastname == lastnameA) %>% select(x_loc,y_loc) 
  dfB <- df %>% filter(lastname == lastnameB) %>% select(x_loc,y_loc) 
  df_l <- 1:nrow(dfA)
  distsdf <- unlist(lapply(df_l,function(x) {dist(rbind(dfA[x,], dfB[x,]))}))
  return(distsdf)
}

get_game_clock <- function(lastnameA,eventID){
  #Function gets the glame clock, assumes there is a dataframe all_movements with player info
  alldf <- all_movements[which((all_movements$lastname == lastnameA) & all_movements$event_id == eventID),]
  game_clock <- alldf$game_clock
  return(as.data.frame(game_clock))
}

player_dist_matrix <- function(eventID) {
  #Function creates a matrix of all player/ball distances with each other
  #assumes there a dataframe all_movements with player info
  players <- all_movements %>% filter(event_id == pickeventID) %>% select(lastname) %>% distinct(lastname)
  players2 <- players
  bigdistance <- unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {test=
      player_dist(X, Y,pickeventID)
    })
  }), recursive=FALSE)
  bigdistance_names <-unlist(lapply(list(players$lastname)[[1]], function(X) {
    lapply(list(players2$lastname)[[1]], function(Y) {
      paste(X, Y,sep = "_")
    })
  }), recursive=FALSE)
  bigdistancedf <- as.matrix(do.call('cbind',bigdistance))
  colnames(bigdistancedf) <- bigdistance_names
  bigdistancedf <- bigdistancedf[,colSums(bigdistancedf^2) !=0]
  bigdistancedf <- as.data.frame(bigdistancedf)
  clockinfo <- get_game_clock("ball",eventID)
  bigdistancedf$game_clock <- clockinfo$game_clock
  return (bigdistancedf)
}

get_pbp <- function(gameid){
  #Grabs the play by play data from the NBA site
  URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
  the_data_file <- fromJSON(URL1)
  test <-the_data_file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the_data_file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)
}

chull_area <- function(X,Y){
  #Calculates the convex hull area
  df_hull <- data.frame(X = X, Y = Y)
  c.hull <- chull(df_hull)
  #You need five points to draw four line segments, so we add the first set of points at the end
  c.hull <- c(c.hull, c.hull[1])
  chull.coords <- df_hull[c.hull ,]
  chull.poly <- Polygon(chull.coords, hole=F)
  chull.area <- chull.poly@area
  return (chull.area)}

chull_areabyteam <- function (total,balltime) {
  #Function returns a dataframe with event id and convex hull area for each team
  #Function requires an input of a dataframe with the rotated plays and a dataframe indicating event/time
  #for calculating convex hull area
  allsum <- NULL
  teams <- as.list((unique(total$team_id)))
  teams <- teams[!is.na(teams)]
  for(i in 1:(nrow(balltime))) 
  {temp <- total %>% filter(event_id == balltime$event_id[i] & game_clock == balltime$clock28[i])  %>% filter(lastname!="ball")
  if (nrow(temp) == 10) {
    dfall <- lapply(teams,function (x) { df <- temp %>% filter(team_id == x)
    if (nrow(df) == 5) {area <- (chull_area(df$x_loc_r,df$y_loc_r))
    area}
    })
    df <- cbind(balltime$event_id[i],teams[[1]],dfall[[1]],teams[[2]],dfall[[2]])  
    allsum <- rbind(df,allsum)
  }
  }
  allsum <- as.data.frame(allsum)
  colnames(allsum)<-c("event_id","team1","team1_area","team2","team2_area")
  return(allsum)
}        

player_position <- function(eventid,gameclock){
  ##Returns positions of all players at a time
  ##Requires data in total and balltime
    dfall <- total %>% filter(game_clock == gameclock,event_id=eventid)  %>% 
      filter(lastname!="ball") %>% select (team_id,x_loc_r,y_loc_r)
    colnames(dfall) <- c('ID','X','Y')
    return(dfall)
  }

chull_plot <- function(event_id,game_clock) {
  ##Returns a data frame with the coordinates of a convex hull
  ##Requires player_position for info
    df2 <- player_position(event_id,game_clock)
    df_hull2 <- df2 %>% filter(ID == min(ID)) %>% select(X,Y)
    df_hull3 <- df2 %>% filter(ID == max(ID)) %>% select(X,Y)
    c.hull2 <- chull(df_hull2)
    c.hull3 <- chull(df_hull3)
    #You need five points to draw four line segments, so we add the fist set of points at the end
    c.hull2 <- c(c.hull2, c.hull2[1])
    c.hull3 <- c(c.hull3, c.hull3[1])
    df2 <- as.data.frame(cbind(1,df_hull2[c.hull2 ,]$X,df_hull2[c.hull2 ,]$Y))
    df3 <- as.data.frame(cbind(2,df_hull3[c.hull3 ,]$X,df_hull3[c.hull3 ,]$Y))
    dfall <- rbind(df2,df3)
    colnames(dfall) <- c('ID','X','Y')
    return(dfall)
  }

chull_plot_centroid <- function(event_id,game_clock) {
  ##Returns a data frame with the centroid of a convex hull
  ##Requires player_position for info
      df2 <- player_position(event_id,game_clock)
      df_hull2 <- df2 %>% filter(ID==min(ID)) %>% select(X,Y)
      df_hull3 <- df2 %>% filter(ID==max(ID)) %>% select(X,Y)
      c.hull2 <- chull(df_hull2)
      c.hull3 <- chull(df_hull3)
      df2centroid <- c(1,mean(df_hull2[c.hull2 ,]$X),mean(df_hull2[c.hull2 ,]$Y))
      df3centroid <- c(2,mean(df_hull3[c.hull3 ,]$X),mean(df_hull3[c.hull3 ,]$Y))
      dfall <- as.data.frame(rbind(df2centroid,df3centroid))
      colnames(dfall) <- c('ID','X','Y')
      return(dfall)
}

chull_plot_area <- function(event_id,game_clock) {
  ##Returns a data frame with the area of each convex hull by team ID
  ##Requires player_position for info
  df2 <- player_position(event_id,game_clock)
  df2area <- df2 %>% group_by(ID) %>% summarise (area = chull_area(X,Y)) %>% select (ID,area)
  return (df2area)
}

velocity <- function(xloc, yloc){
  diffx <- as_vector((diff(xloc)))
  diffy <- as_vector((diff(yloc)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25 #(distance in feet per second)
  b
}

acceleration <- function(xloc, yloc){
  diffx <- as.vector((diff(xloc,differences = 2)))
  diffy <- as.vector((diff(yloc,differences = 2)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25*25 #(distance in feet per second)
  b
}

jerk <- function(xloc, yloc){
  diffx <- as.vector((diff(xloc,differences = 3)))
  diffy <- as.vector((diff(yloc,differences = 3)))
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)*25/32.173*25*25 #(distance in feet per second)
  b
}

