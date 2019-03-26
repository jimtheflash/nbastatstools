#' List active players by games and teams
#' @param boxscores data.frame of boxscores
#' @return list of games with lists of rosters
#' @export
make_team_game_player_rosters <- function(boxscores = NULL) {
  # loop through games
  output_list <- list()
  games <- unique(boxscores$game_id)
  for (game in games) {
    # for each game, store players for each team
    game_subset <- dplyr::filter(boxscores, game_id == game)
    game_list <- list()
    teams <- unique(game_subset$team_id)
    game_list$teams <- teams
    for (team in teams) {
      team_subset <- dplyr::filter(game_subset, team_id == team)
      game_list[[team]] <- unique(team_subset$player_id[team_subset$min > 0])
    }
    output_list[[game]] <- game_list
  }
  return(output_list)
}