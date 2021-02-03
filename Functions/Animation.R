

library(tidyverse)
library(teamcolors)
library(nbastatR)

### Get team colors and names
team_names <- read_rds(here("Data/team_id.rds"))
team_colors <- teamcolors %>% filter(league == "nba")
color_names <- team_names %>%
  mutate(idTeam = factor(idTeam)) %>%
  left_join(select(team_colors, primary, logo, name), by = c("nameTeam" = "name")) 
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
    left_join(select(color_names, primary, nameTeam, logo, idTeam), by = c("team_id" = "idTeam"))
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
  # Creating the plot that is a basis for animating
  p2 <- fullcourt(court_themes$light) +
    # Add Players
    geom_point(data = filter(anim_x, value == "player"), 
               aes(x = x_loc, y = y_loc, group = team_id, color = team_id), size = 5) +
    geom_text(data = filter(anim_x, value == "player"), 
              aes(x = x_loc, y = y_loc, group = team_id, label = paste(firstname, lastname)), 
              vjust = -1, color = "black", family = "Roboto Condensed") +
    scale_color_manual(values = c(dplyr::filter(anim_394, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),  
                                  dplyr::filter(anim_394, team_value == "home") %>% select(primary) %>% unique() %>% deframe()),
                       labels = c(dplyr::filter(anim_394, team_value == "away") %>% select(nameTeam) %>% unique() %>% deframe(),
                                  dplyr::filter(anim_394, team_value == "home") %>% select(nameTeam) %>% unique() %>% deframe())) +
    geom_point(data = filter(anim_x, value == "ball"), color = "darkorange", size = 3, aes(x = x_loc, y = y_loc)) + # Add ball
    # Add Convex hull areas
    geom_polygon(data = chull_x, aes(x = x_loc, y = y_loc, group = team_id, fill = team_id),
                 alpha = 0.2, show.legend = F) +
    scale_fill_manual(values = c(dplyr::filter(anim_394, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),
                                 dplyr::filter(anim_394, team_value == "home") %>% select(primary) %>% unique() %>% deframe())) +
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
    filter(row_number() %% 2 == 0)
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
  # Creating the plot that is a basis for animating
  p2 <- fullcourt(court_themes$light) +
    # Add Players
    geom_point(data = filter(anim_x, value == "player"), 
               aes(x = x_loc, y = y_loc, group = team_id, color = team_id), size = 5) +
    geom_text(data = filter(anim_x, value == "player"), 
              aes(x = x_loc, y = y_loc, group = team_id, label = paste(firstname, lastname)), 
              vjust = -1, color = "black", family = "Roboto Condensed") +
    scale_color_manual(values = c(dplyr::filter(anim_394, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),  
                                  dplyr::filter(anim_394, team_value == "home") %>% select(primary) %>% unique() %>% deframe()),
                       labels = c(dplyr::filter(anim_394, team_value == "away") %>% select(nameTeam) %>% unique() %>% deframe(),
                                  dplyr::filter(anim_394, team_value == "home") %>% select(nameTeam) %>% unique() %>% deframe())) +
    geom_point(data = filter(anim_x, value == "ball"), color = "darkorange", size = 3, aes(x = x_loc, y = y_loc)) + # Add ball
    # Add Convex hull areas
    geom_polygon(data = chull_x, aes(x = x_loc, y = y_loc, group = team_id, fill = team_id),
                 alpha = 0.2, show.legend = F) +
    scale_fill_manual(values = c(dplyr::filter(anim_394, team_value == "away") %>% select(primary) %>% unique() %>% deframe(),
                                 dplyr::filter(anim_394, team_value == "home") %>% select(primary) %>% unique() %>% deframe())) +
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
                      nframes = 100, # Rounds to the nearest 10th
                      fps = 20,
                      res = 100,
                      end_pause = 0)
  animated
}
