#' Identify players who are leading team in stat categories for a season coming into a game
#' @importFrom magrittr %>%
#' @param boxscores data.frame of boxscore info
#' @param stat_list list of string names of fields to calc leaders on
#' @return data.frame of team-season-cumulative-leaders
#' @export
identify_team_season_cumulative_leaders <- function(boxscores = NULL,
                                                    stat_list = c("min", "pts", "fgthreem", "reb", "ast", "plus_minus")) {
  # loop through teams, calculate cumulative team-season leaders by game
  teams <- unique(boxscores$team_id)
  output_list <- list()
  for (team in teams) {
    subset_team <- dplyr::filter(boxscores, team_id == team)
    # loop through seasons for the team
    team_seasons <- unique(subset_team$season)
    for (team_season in team_seasons) {
      subset_season <- subset_team %>%
        dplyr::filter(season == team_season) %>%
        dplyr::arrange(player_id, game_date) %>%
        dplyr::group_by(player_id) %>%
        dplyr::mutate_at(.vars = dplyr::vars(stat_list),
                         .funs = list(season_cumsum = ~cumsum(.))) %>%
        dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("season_cumsum")),
                         .funs = list(prev_game = ~dplyr::lag(.))) %>%
        dplyr::ungroup() %>%
        dplyr::select(game_id, player_id,
                      dplyr::ends_with("prev_game"))
      # loop through the stats list and identify player leader entering each game
      season_output <- data.frame(game_id = unique(subset_season$game_id),
                                  team_id = team,
                                  stringsAsFactors = FALSE)
      for (stat in stat_list) {
        stat_pattern <- paste0(stat, "_")
        stat_col <- names(subset_season)[grepl(stat_pattern, 
                                               names(subset_season))]
        stat_df <- subset_season %>%
          dplyr::select(game_id, player_id, !!stat_col) %>%
          tidyr::spread(player_id, !!stat_col, sep = "_")
        incoming_game_leader <- 
          apply(stat_df[, names(stat_df) != "game_id"], 1, 
            function(x) attr(which.max(x), "names")) %>%
          lapply(., function(y) ifelse(is.null(y), NA_character_, y)) %>%
          unlist() %>%
          gsub("player_id_", "", .)
        stat_output <- data.frame(game_id = stat_df$game_id,
                                  stringsAsFactors = FALSE)
        output_col <- paste0("incoming_", stat, "_leader")
        stat_output[[output_col]] <- incoming_game_leader
        season_output <- dplyr::inner_join(season_output, stat_output,
                                           by = "game_id")
      }
      # stick the season_output into the output_list
      output_list[[length(output_list) + 1]] <- season_output
    }
  }
  # turn that list into a data.frame and return
  output_df <- dplyr::bind_rows(output_list)
  return(output_df)
}
    
