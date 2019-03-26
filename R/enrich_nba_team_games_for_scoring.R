#' Enrich NBA team-game data for scoring
#' @importFrom magrittr %>%
#' @param game_slate data.frame output of \code{nbastatstools::get_nba_game_slate()}
#' @param team_games_for_scoring data.frame of team-games-to-be-scored
#' @return data.frame
#' @export
enrich_nba_team_games_for_scoring <- function(game_slate = NULL,
                                              team_games_for_scoring = NULL) {
  
  #### get necessary info for matching ####
  # extract game_date for easier labeling
  current_game_date <- unique(game_slate$game_date)
  
  # get games for tomorrow to calculate backtobacks
  next_games <- 
    nbastatstools::get_nba_game_slate(
      game_date = as.Date(current_game_date + 1)) %>%
    nbastatstools::tidy_nba_game_slate()
  
  # enrich games for scoring
  enriched <- nbastatstools::enrich_nba_team_games(team_games_for_scoring)
  
  # filter out columns that either need to be excluded or re-engineered
  filtered <- enriched %>%
    dplyr::select(-dplyr::ends_with("prev_game"),
                  -dplyr::matches("backtoback"),
                  -dplyr::matches("home_away"),
                  -dplyr::matches("wl"))
  # rename features
  new_names <- ifelse(
    !grepl("_id|game_date|season|prev_season|backtoback|home_win", 
           names(filtered)),
    paste0(names(filtered), "_prev_game"),
    names(filtered))
  names(filtered) <- new_names
  
  #### loop through the game_slate and stitch together inputs to be preprocessed ####
  slate_inputs <- list()
  for (i in 1:nrow(game_slate)) {
    
    # get info from target game in slate
    target_game <- game_slate[i, ]
    target_home_team <- target_game$home_team_id
    target_visitor_team <- target_game$visitor_team_id
    
    #### get prev_game info for target_home_team ####
    home_team_prev_game <- filtered %>%
      dplyr::filter(filtered$home_team_id == target_home_team | 
                      filtered$visitor_team_id == target_home_team) %>%
      dplyr::filter(game_date == max(game_date))
    ### if the target_home_team's prev_game was home, get the home fields, else the visitor fields
    if (home_team_prev_game$home_team_id == target_home_team) {
      home_inputs <- home_team_prev_game %>%
        dplyr::select(game_date,
                      dplyr::starts_with("home")) %>%
        dplyr::mutate(home_home_away_prev_game = "home",
                      home_wl_prev_game = dplyr::if_else(home_win == "win", "win", "loss")) %>%
        dplyr::select(-home_win)
    } else {
      home_inputs <- home_team_prev_game %>%
        dplyr::select(game_date,
                      home_win,
                      home_margin_prev_game,
                      dplyr::starts_with("visitor")) %>%
        dplyr::mutate(home_margin_prev_game = home_margin_prev_game * -1,
                      home_home_away_prev_game = "away",
                      home_wl_prev_game =
                        dplyr::if_else(home_win == "loss", "win", "loss")) %>%
        dplyr::select(-home_win, -home_margin_prev_game)
      new_home_team_names <- gsub("visitor", "home", names(home_inputs))
      names(home_inputs) <- new_home_team_names
    }
    ### re-engineer backtoback info
    home_inputs <- home_inputs %>%
      dplyr::mutate(
        home_is_backtoback_back = 
          dplyr::if_else(game_date == current_game_date - 1, 1, 0),
        home_is_backtoback_front =
          dplyr::if_else(target_home_team %in% next_games$home_team_id |
                           target_home_team %in% next_games$visitor_team_id,
                         1, 0)) %>%
      dplyr::mutate(
        home_is_backtoback =
          dplyr::if_else(
            home_is_backtoback_back == 1 | home_is_backtoback_front == 1, 1, 0)) %>%
      dplyr::select(-game_date)
    
    #### get prev_game info for target_visitor_team ####
    visitor_team_prev_game <- filtered %>%
      dplyr::filter(filtered$home_team_id == target_visitor_team | 
                      filtered$visitor_team_id == target_visitor_team) %>%
      dplyr::filter(game_date == max(game_date))
    ### if the target_home_team's prev_game was home, get the home fields, else the visitor fields
    if (visitor_team_prev_game$home_team_id == target_visitor_team) {
      visitor_inputs <- visitor_team_prev_game %>%
        dplyr::select(game_date,
                      dplyr::starts_with("home")) %>%
        dplyr::mutate(visitor_home_away_prev_game = "home",
                      visitor_wl_prev_game = dplyr::if_else(home_win == "win", "win", "loss")) %>%
        dplyr::select(-home_win)
      new_visitor_team_names <- gsub("^home", "visitor", names(visitor_inputs))
      names(visitor_inputs) <- new_visitor_team_names
    } else {
      visitor_inputs <- visitor_team_prev_game %>%
        dplyr::select(game_date,
                      home_win,
                      home_margin_prev_game,
                      dplyr::starts_with("visitor")) %>%
        dplyr::mutate(visitor_margin_prev_game = home_margin_prev_game * -1,
                      visitor_home_away_prev_game = "away",
                      visitor_wl_prev_game = dplyr::if_else(home_win == "loss", "win", "loss")) %>%
        dplyr::select(-home_win, -home_margin_prev_game)
    }
    ### re-engineer backtoback info
    visitor_inputs <- visitor_inputs %>%
      dplyr::mutate(
        visitor_is_backtoback_back = dplyr::if_else(game_date == current_game_date - 1, 1, 0),
        visitor_is_backtoback_front = dplyr::if_else(target_home_team %in% next_games$home_team_id |
                                                       target_home_team %in% next_games$visitor_team_id,
                                                     1, 0)) %>%
      dplyr::mutate(visitor_is_backtoback = dplyr::if_else(
        visitor_is_backtoback_back == 1 | visitor_is_backtoback_front == 1, 1, 0)
      ) %>%
      dplyr::select(-game_date)
    #### combine new home and visitor data ####
    new_input <- dplyr::bind_cols(home_inputs, visitor_inputs)
    new_input$game_id <- target_game$game_id
    new_input$game_date <- target_game$game_date
    slate_inputs[[i]] <- new_input
  }
  # stitch everything back together
  enriched_team_games_for_scoring <- dplyr::bind_rows(slate_inputs)
  # increment game numbers
  enriched_team_games_for_scoring$home_season_game_num <-
    enriched_team_games_for_scoring$home_season_game_num + 1
  enriched_team_games_for_scoring$visitor_season_game_num <-
    enriched_team_games_for_scoring$visitor_season_game_num + 1
  return(enriched_team_games_for_scoring)
}
