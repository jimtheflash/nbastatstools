#' tidy NBA team-game scores
#' @param scored_team_games list of model outputs from \code{nbastatstools::score_nba_team_games()}
#' @param game_slate game slate output from \code{nbastatstools::tidy_game_slate()}
#' @param boxscores_for_scoring boxscore data to extract team abbreviations
#' @return tidy data.frame of nba team-game scores
#' @export
tidy_nba_team_game_scores <- function(scored_team_games = NULL,
                                      game_slate = NULL,
                                      boxscores_for_scoring = NULL) {
  # loop through scored games and combine all the preds into a wide table
  model_names <- names(scored_team_games)
  tidy_model_game_scores <- list()
  for (mod in model_names) {
    # stack all the targets together
    long_df <- dplyr::bind_rows(scored_team_games[[mod]])
    # widen by targets
    wider_df <- tidyr::spread(long_df, target, preds)
    # tie to game slates
    tidy_game_scores <- game_slate %>%
      dplyr::inner_join(wider_df)
    tidy_model_game_scores[[mod]] <- tidy_game_scores
  }
  # combine the wide tables to get all the games in play today
  all_tidy_games <- dplyr::bind_rows(tidy_model_game_scores)
  # enrich with team abbreviations
  home_team_lu <- boxscores_for_scoring %>%
    dplyr::filter(team_id %in% unique(all_tidy_games$home_team_id)) %>%
    dplyr::group_by(team_id, team_abbreviation) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::transmute(home_team_id = team_id,
                     home_team_abbrev = team_abbreviation)
  all_tidy_games <- dplyr::inner_join(all_tidy_games, home_team_lu)
  visitor_team_lu <- boxscores_for_scoring %>%
    dplyr::filter(team_id %in% unique(all_tidy_games$visitor_team_id)) %>%
    dplyr::group_by(team_id, team_abbreviation) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::transmute(visitor_team_id = team_id,
                     visitor_team_abbrev = team_abbreviation)
  all_tidy_games <- dplyr::inner_join(all_tidy_games, visitor_team_lu)
  # re-order columns for readability
  id_cols <- c("game_id", "game_date",
               "home_team_id", "home_team_abbrev",
               "visitor_team_id", "visitor_team_abbrev")
  all_tidy_games <- dplyr::select(all_tidy_games, id_cols) %>%
    dplyr::bind_cols(dplyr::select(all_tidy_games, -id_cols))
  return(all_tidy_games)
}

