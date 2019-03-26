#' Indicate if a team-season-leader in a stat category is in lineup for game
#' @importFrom magrittr %>%
#' @param boxscores data.frame of boxscores
#' @return data.frame
#' @export
identify_team_season_leaders_in_lineup <- function(boxscores = NULL) {
  # get team-game player lists
  player_lists <- 
    nbastatstools::make_team_game_player_rosters(boxscores = boxscores)
  # get team-season leaders
  team_game_leaders <- 
    nbastatstools::identify_team_season_cumulative_leaders(boxscores = boxscores)
  names(team_game_leaders) <- gsub("incoming_", "", names(team_game_leaders))
  names(team_game_leaders) <- gsub("_leader", "", names(team_game_leaders))
  stat_names <- names(team_game_leaders)[!grepl("_id$", names(team_game_leaders))]
  # loop through games and identify when stat leaders are missing
  output_list <- list()
  for (i in 1:nrow(team_game_leaders)) {
    team_game <- team_game_leaders[i, ]
    game <- team_game$game_id
    team <- team_game$team_id
    player_list_subset <- player_lists[[game]][[team]]
    team_game_list <- list()
    for (stat in stat_names) {
      team_game_list[[stat]] <- team_game[[stat]] %in% player_list_subset
    }
    team_game_df <- dplyr::bind_rows(team_game_list) %>%
      dplyr::mutate(game_id = game,
                    team_id = team)
    output_list[[length(output_list) + 1]] <- team_game_df
  }
  # convert to df, update column names and classes
  output_df <- dplyr::bind_rows(output_list)
  new_names <-
    paste0("season_leader_", names(output_df)[!grepl("_id", names(output_df))],
           "_in_lineup")
  names(output_df)[!grepl("_id", names(output_df))] <- new_names
  output_df <- apply(output_df[, !grepl("_id", names(output_df))],
                     2, as.numeric) %>%
    as.data.frame(stringsAsFactors = FALSE)
  # return
  return(output_df)
}