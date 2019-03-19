#' Enrich NBA team-game data for scoring
#' @importFrom magrittr %>%
#' @param game_slate data.frame output of \code{nbastatstools::get_nba_game_slate()}
#' @param team_games_for_scoring data.frame of team-games-to-be-scored
#' @return data.frame
#' @export
enrich_nba_team_games_for_scoring <- function(game_slate = NULL,
                                              team_games_for_scoring = NULL) {
  
  # extract game_date for easier labeling
  current_game_date <- unique(game_slate$game_date)
  # enrich
  enriched <- nbastatstools::enrich_nba_team_games(team_games_for_scoring)
  # filter out columns that either need to be excluded or re-engineered
  enriched <- enriched %>%
    dplyr::select(-dplyr::ends_with("matchup"),
                  -dplyr::ends_with("prev_game"),
                  -dplyr::matches("cummean"),
                  -dplyr::matches("cumsum"),
                  -dplyr::matches("dow"),
                  -dplyr::matches("game_id"),
                  -dplyr::matches("month"),
                  -dplyr::matches("opposing"),
                  -dplyr::matches("season_id"))
  # append suffixes to targets to align with expected column names in preprocessing
  new_names <- 
    ifelse(grepl("five|ten|twenty|_team|_opp|_used|margin|wl|home_away", 
                            names(enriched)),
                            paste0(names(enriched), "_prev_game"),
                            names(enriched)) %>%
    gsub("_prev_game_prev_game", "_prev_game", .) %>%
    gsub("season_total_prev_game", "season_total", .) %>%
    gsub("season_mean_prev_game", "season_mean", .)
  names(enriched) <- new_names
  # re-engineer some columns
  enriched <- enriched %>%
    dplyr::mutate(dow = weekdays(current_game_date),
                  month = months(current_game_date),
                  home_away = 
                    dplyr::if_else(team_id %in% game_slate$home_team_id,
                                   "home", "away"))
  # restructure game_slate for for each matchup
  matchups <- dplyr::bind_rows(game_slate,
                               game_slate) %>%
    dplyr::mutate(team_id = 
                    c(game_slate$home_team_id, game_slate$visitor_team_id),
                  opponent_team_id = 
                    c(game_slate$visitor_team_id, game_slate$home_team_id))
  # get games to be played on next date for identifying backtoback games
  next_games <- 
    nbastatstools::get_nba_game_slate(game_date = 
                                        as.Date(current_game_date + 1)) %>%
    nbastatstools::tidy_nba_game_slate()
  output_data <- list()
  # loop through matchup teams and stitch together data for preprocessing
  teams_in_slate <- unique(matchups$team_id)
  teams_playing_tomorrow <- unique(c(next_games$home_team_id, 
                                     next_games$visitor_team_id))
  for (team in teams_in_slate) {
    # subset for the team to score, and get the game info and metadata
    enriched_team <- enriched %>%
      dplyr::filter(team_id == team) %>%
      dplyr::filter(game_date == max(game_date)) %>%
      dplyr::mutate(team_season_game_num = 
                      nrow(enriched %>%
                             dplyr::filter(team_id == team,
                                           season == max(season))) + 1,
                    is_backtoback_front = 
                      dplyr::if_else(team_id %in% teams_playing_tomorrow, 1, 0),
                    is_backtoback_back = 
                      dplyr::if_else(max(game_date) == 
                                       as.Date(current_game_date - 1),
                                     1, 0)) %>%
      dplyr::mutate(is_backtoback = dplyr::if_else(is_backtoback_front == 1 | 
                                                   is_backtoback_back == 1,
                                                   1, 0))
    # subset the opposing team to score
    opposing_enriched_team <- enriched %>%
      dplyr::filter(team_id == 
                      matchups$opponent_team_id[matchups$team_id == team]) %>%
      dplyr::filter(game_date == max(game_date))
    new_names <- paste0("opposing_team_", names(opposing_enriched_team))
    names(opposing_enriched_team) <- new_names
    # stitch together columns
    enriched_team_game_for_preprocessing <- 
      dplyr::bind_cols(enriched_team, opposing_enriched_team)
    # output
    output_data[[length(output_data) + 1]] <- 
      enriched_team_game_for_preprocessing
  }
  # make the output a single data.frame and update a few columns
  output_df <- dplyr::bind_rows(output_data)
  output_df$game_date <- current_game_date
  output_df$game_id <- matchups$game_id
  return(output_df)
}
