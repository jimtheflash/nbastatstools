identify_team_game_leaders <- function(boxscores = NULL) {
  # loop through teams and games, and identify leaders across stats categories
  # include: min, pts, fgthreem, reb, ast, plus_minus
  team_game_leaders <- list()
  teams <- unique(boxscores$team_id)
  for (team in teams) {
    # subset for another loop
    team_boxscores <- dplyr::filter(boxscores, team_id == team)
    team_games <- list()
    games <- unique(team_boxscores$game_id)
    for (game in games) {
      # subset
      team_game_boxscore <- dplyr::filter(team_boxscores, game_id == game)
      # identify team stat maximums
      max_minutes <- max(team_game_boxscore$min, na.rm = TRUE)
      max_pts <- max(team_game_boxscore$pts, na.rm = TRUE)
      max_fgthreem <- ifelse(is.na(max(team_game_boxscore$fgthreem, na.rm = TRUE)),
                             0, max(team_game_boxscore$fgthreem, na.rm = TRUE))
      max_reb <- max(team_game_boxscore$reb, na.rm = TRUE)
      max_ast <- max(team_game_boxscore$ast, na.rm = TRUE)
      max_plus_minus <- max(team_game_boxscore$plus_minus, na.rm = TRUE)
      # identify team leaders
      
                             
      
      
    }
  }
}