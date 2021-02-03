

library(tidyverse)
library(teamcolors)
library(nbastatR)
library(magick)

### Get team colors and names
team_logos <- tibble(logo = c("https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/logo-golden-state-warriors.png?w=1000&h=600&crop=1",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/primary.jpg",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/denver_nuggets-svg.png?resize=1024,721",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/memphis_grizzlies-svg.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/celtics-logos.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/bullslogo.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/screen-shot-2016-05-27-at-9-51-26-am.jpg?resize=1024,925",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/hornets_primary_logo.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/newyorkknicks.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/minnesota_timberwolves-svg.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-30-39-pm.jpg?resize=1024,1022",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/2000px-losangeles_lakers_logo-svg.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/detroit_pistons_logo-svg.png?resize=1024,853",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/indiana_pacers-svg.png?resize=1024,889",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/nets_logo.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/cleveland_cavaliers_2010-svg.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/5671_washington_wizards-primary-2016.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/76ers_2015_logo_detail.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-40-38-pm.jpg?resize=1024,800",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/kings-logo.jpg",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-35-21-pm.jpg",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/oklahoma_city_thunder-svg.png?resize=1024,938",
                              "https://wp.usatodaysports.com/wp-content/uploads/sites/90/2016/05/wd9ic7qafgfb0yxs7tem7n5g4.gif",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-34-36-pm.jpg",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-31-28-pm.jpg?resize=1024,971",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-38-10-pm.jpg",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/1280px-san_antonio_spurs-svg.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/05/download1.png",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-36-34-pm.jpg?resize=834,1024",
                              "https://ftw.usatoday.com/wp-content/uploads/sites/90/2016/04/screen-shot-2016-04-26-at-2-43-19-pm.jpg?resize=768,907"
                              ),
                     name = c("Golden State Warriors",
                              "Atlanta Hawks",
                              "Denver Nuggets",
                              "Memphis Grizzlies",
                              "Boston Celtics",
                              "Chicago Bulls",
                              "Milwaukee Bucks",
                              "Charlotte Hornets",
                              "New York Knicks",
                              "Minnesota Timberwolves",
                              "New Orleans Pelicans",
                              "Los Angeles Lakers",
                              "Detroit Pistons",
                              "Indiana Pacers",
                              "Brooklyn Nets",
                              "Cleveland Cavaliers",
                              "Washington Wizards",
                              "Philadelphia 76ers",
                              "Los Angeles Clippers",
                              "Sacramento Kings",
                              "Dallas Mavericks",
                              "Oklahoma City Thunder",
                              "Orlando Magic",
                              "Houston Rockets",
                              "Toronto Raptors",
                              "Phoenix Suns",
                              "San Antonio Spurs",
                              "Utah Jazz",
                              "Miami Heat",
                              "Portland Trail Blazers"
                              ))
team_names <- read_rds(here("Data/team_id.rds"))
team_colors <- teamcolors %>% filter(league == "nba")
color_names <- team_names %>%
  mutate(idTeam = factor(idTeam)) %>%
  left_join(select(team_colors, primary, name), by = c("nameTeam" = "name")) %>%
  left_join(team_logos, by = c("nameTeam" = "name"))
# Logo could be useful for annotation_custom in center later?

### ANIMATION FUNCTION WITH NUMBER OF FRAMES THAT IS ONE FRAME FOR EACH OF THE OBSERVATIONS IN THE DATAFRAME

animate_play <- function(DF, EVENT_ID) {
  # Select event
  event_x <- DF %>% 
    filter(event_id == EVENT_ID) %>%
    arrange(desc(game_clock))
  # Select max time to subset when gif should end, most go on too long
  # balltime <- event_x %>%
  #   filter(player_id == -1) %>%
  #   summarise(clock28 = max(game_clock[x_loc < 28])) %>%
  #   deframe()
  # Dataframe for play 394, groups by shot clock to make group id and identifies ball and player
  anim_x <- event_x %>%
    mutate(group_id = dense_rank(desc(game_clock))) %>%
    relocate(group_id, .before = event_id) %>%
    mutate(value = case_when(player_id == -1 ~ "ball",
                             player_id != -1 ~ "player"),
           team_id = factor(team_id),
           quarter_annotate = case_when(quarter == 4 ~ "4th",
                                        quarter == 3 ~ "3rd",
                                        quarter == 2 ~ "2nd",
                                        quarter == 1 ~ "1st",
                                        quarter == 5 ~ "5th",
                                        quarter == 6 ~ "6th",
                                        quarter == 7 ~ "7th",
                                        quarter == 8 ~ "8th"),
           clock_annotate = game_clock_reformat(game_clock)) %>%
    left_join(select(color_names, primary, nameTeam, logo, idTeam), by = c("team_id" = "idTeam")) %>%
    group_by(group_id) %>%
    filter(row_number() %% 2 == 0) %>%
    ungroup()
  # Calculating the chull
  chull_x <- anim_x %>%
    filter(value != "ball") %>%
    group_by(group_id, team_id) %>%
    nest() %>%
    mutate(
      hull = map(data, ~ with(.x, chull_plot2(x_loc, y_loc))),
      out = map2(data, hull, ~ .x[.y, , drop = F])
    ) %>%
    select(-data) %>%
    unnest(cols = c(hull, out)) %>%
    group_by(group_id, team_id) %>%
    mutate(centroid_x = mean(x_loc),
           centroid_y = mean(y_loc)) %>%
    ungroup()
  # Getting the home team logo
  img <- (dplyr::filter(anim_x, team_value == "home") %>% select(logo) %>% unique() %>% pull(logo))
  logo_raw <- magick::image_read(img) %>%
    image_transparent(color = "white")
  logo <- rasterGrob(logo_raw, interpolate = T)
  # Creating the plot that is a basis for animating
  p2 <- fullcourt(court_themes$light) +
    # Add logo
    annotation_custom(logo, xmin = 42, xmax = 52, ymin = 20.02, ymax = 30.02) +
    # Add Players
    geom_point(data = filter(anim_x, value == "player"), 
               aes(x = x_loc, y = y_loc, group = team_id, color = team_id), size = 5) +
    geom_text(data = filter(anim_x, value == "player"), 
              aes(x = x_loc, y = y_loc, group = team_id, label = paste(firstname, lastname)), 
              vjust = -1, color = "black", family = "Roboto Condensed") +
    scale_color_manual(values = c(dplyr::filter(anim_x, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),  
                                  dplyr::filter(anim_x, team_value == "home") %>% select(primary) %>% unique() %>% deframe()),
                       labels = c(dplyr::filter(anim_x, team_value == "away") %>% select(nameTeam) %>% unique() %>% deframe(),
                                  dplyr::filter(anim_x, team_value == "home") %>% select(nameTeam) %>% unique() %>% deframe())) +
    geom_point(data = filter(anim_x, value == "ball"), color = "darkorange", size = 3, aes(x = x_loc, y = y_loc)) + # Add ball
    # Add Convex hull areas
    geom_polygon(data = chull_x, aes(x = x_loc, y = y_loc, group = team_id, fill = team_id),
                 alpha = 0.2, show.legend = F) +
    scale_fill_manual(values = c(dplyr::filter(anim_x, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),
                                 dplyr::filter(anim_x, team_value == "home") %>% select(primary) %>% unique() %>% deframe())) +
    # Add Centroids
    geom_point(data = chull_x, aes(x = centroid_x, y = centroid_y, group = team_id, color = team_id),
               size = 3, shape = 13) +
    geom_text(data = chull_x, aes(x = centroid_x, y = centroid_y, group = team_id, color = team_id),
              size = 3, label = "Centroid:", hjust = 1.2, show.legend = F, family = "Roboto Condensed") +
    # Add shot clock
    geom_text(data = anim_x, aes(x = 64.5, y = 57.5, label = shot_clock), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") + # Why is geom_text always so ugly
    # Add game clock
    geom_text(data = anim_x, aes(x = 58.25, y = 57.5, label = clock_annotate), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") +
    # Add quarter
    geom_text(data = anim_x, aes(x = 50.75, y = 57.5, label = str_to_upper(quarter_annotate)), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") +
    ggtitle(paste("Current Score:", anim_x$slugScore))
  anim_final <- p2 + transition_manual(frames = group_id)
  animated <- animate(anim_final, 
                           width = 1725.450,
                           height = 913.636,
                           nframes = (length(unique(anim_x$group_id))),
                           fps = 25,
                           res = 100,
                           end_pause = 0)
  return(animated)
}

### ANIMATION FUNCTION WITH NUMBER OF FRAMES THAT IS ONE HALF OF THE OBSERVATIONS IN THE DATAFRAME

animate_play_quick <- function(DF, EVENT_ID) {
  # Select event
  event_x <- DF %>% 
    filter(event_id == EVENT_ID) %>%
    arrange(desc(game_clock))
  # Select max time to subset when gif should end, most go on too long
  # balltime <- event_x %>%
  #   filter(player_id == -1) %>%
  #   summarise(clock28 = max(game_clock[x_loc < 28])) %>%
  #   deframe()
  # Dataframe for play 394, groups by shot clock to make group id and identifies ball and player
  anim_x <- event_x %>%
    mutate(group_id = dense_rank(desc(game_clock))) %>%
    relocate(group_id, .before = event_id) %>%
    mutate(value = case_when(player_id == -1 ~ "ball",
                             player_id != -1 ~ "player"),
           team_id = factor(team_id),
           quarter_annotate = case_when(quarter == 4 ~ "4th",
                                        quarter == 3 ~ "3rd",
                                        quarter == 2 ~ "2nd",
                                        quarter == 1 ~ "1st",
                                        quarter == 5 ~ "5th",
                                        quarter == 6 ~ "6th",
                                        quarter == 7 ~ "7th",
                                        quarter == 8 ~ "8th"),
           clock_annotate = game_clock_reformat(game_clock)) %>%
    left_join(select(color_names, primary, nameTeam, logo, idTeam), by = c("team_id" = "idTeam")) %>%
    group_by(group_id) %>%
    filter(row_number() %% 2 == 0) %>%
    ungroup()
  # Calculating the chull
  chull_x <- anim_x %>%
    filter(value != "ball") %>%
    group_by(group_id, team_id) %>%
    nest() %>%
    mutate(
      hull = map(data, ~ with(.x, chull_plot2(x_loc, y_loc))),
      out = map2(data, hull, ~ .x[.y, , drop = F])
    ) %>%
    select(-data) %>%
    unnest(cols = c(hull, out)) %>%
    group_by(group_id, team_id) %>%
    mutate(centroid_x = mean(x_loc),
           centroid_y = mean(y_loc)) %>%
    ungroup()
  # Getting the home team logo
  img <- (dplyr::filter(anim_x, team_value == "home") %>% select(logo) %>% unique() %>% pull(logo))
  logo_raw <- magick::image_read(img) %>%
    image_transparent(color = "white")
  logo <- rasterGrob(logo_raw, interpolate = T)
  # Creating the plot that is a basis for animating
  p2 <- fullcourt(court_themes$light) +
    # Add logo
    annotation_custom(logo, xmin = 42, xmax = 52, ymin = 20.02, ymax = 30.02) +
    # Add Players
    geom_point(data = filter(anim_x, value == "player"), 
               aes(x = x_loc, y = y_loc, group = team_id, color = team_id), size = 5) +
    geom_text(data = filter(anim_x, value == "player"), 
              aes(x = x_loc, y = y_loc, group = team_id, label = paste(firstname, lastname)), 
              vjust = -1, color = "black", family = "Roboto Condensed") +
    scale_color_manual(values = c(dplyr::filter(anim_x, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),  
                                  dplyr::filter(anim_x, team_value == "home") %>% select(primary) %>% unique() %>% deframe()),
                       labels = c(dplyr::filter(anim_x, team_value == "away") %>% select(nameTeam) %>% unique() %>% deframe(),
                                  dplyr::filter(anim_x, team_value == "home") %>% select(nameTeam) %>% unique() %>% deframe())) +
    geom_point(data = filter(anim_x, value == "ball"), color = "darkorange", size = 3, aes(x = x_loc, y = y_loc)) + # Add ball
    # Add Convex hull areas
    geom_polygon(data = chull_x, aes(x = x_loc, y = y_loc, group = team_id, fill = team_id),
                 alpha = 0.2, show.legend = F) +
    scale_fill_manual(values = c(dplyr::filter(anim_x, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),
                                 dplyr::filter(anim_x, team_value == "home") %>% select(primary) %>% unique() %>% deframe())) +
    # Add Centroids
    geom_point(data = chull_x, aes(x = centroid_x, y = centroid_y, group = team_id, color = team_id),
               size = 3, shape = 13) +
    geom_text(data = chull_x, aes(x = centroid_x, y = centroid_y, group = team_id, color = team_id),
              size = 3, label = "Centroid:", hjust = 1.2, show.legend = F, family = "Roboto Condensed") +
    # Add shot clock
    geom_text(data = anim_x, aes(x = 64.5, y = 57.5, label = shot_clock), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") + # Why is geom_text always so ugly
    # Add game clock
    geom_text(data = anim_x, aes(x = 58.25, y = 57.5, label = clock_annotate), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") +
    # Add quarter
    geom_text(data = anim_x, aes(x = 50.75, y = 57.5, label = str_to_upper(quarter_annotate)), 
              size = 6, color = "#BF0A30", family = "Roboto Condensed", fontface = "bold") +
    ggtitle(paste("Current Score:", anim_x$slugScore))
  anim_final <- p2 + transition_manual(frames = group_id)
  animated <- animate(anim_final, 
                      width = 1725.450,
                      height = 913.636,
                      nframes = (length(unique(anim_x$group_id))), # Total number of frames in subset data - which should be half
                      fps = 20,
                      res = 100,
                      end_pause = 0)
  animated
}
