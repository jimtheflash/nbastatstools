#' make a team-games table from nba stats api boxscores
#' @importFrom magrittr %>%
#' @param boxscores data.frame of boxscores from nba stats api
#' @return data.frame of team_game info for easy joining
#' @export
make_nba_team_games <- function(boxscores = NULL) {
  message("building team games from boxscores...")
  team_games <- boxscores %>%
    dplyr::group_by(team_id, game_id, season_label, game_date, home_away) %>%
    dplyr::summarise_at(.vars = dplyr::vars(min,
                                            dplyr::starts_with("fg"),
                                            dplyr::starts_with("ft"),
                                            dplyr::ends_with("reb"),
                                            ast,
                                            stl,
                                            blk,
                                            tov,
                                            pf,
                                            pts), 
                        .funs = dplyr::funs(team = sum)) %>%
    dplyr::ungroup() %>%
    # add additional summary measures
    dplyr::inner_join(boxscores %>%
                        dplyr::group_by(team_id, game_id) %>%
                        dplyr::summarise(players_used = dplyr::n_distinct(player_id)) %>%
                        dplyr::ungroup(),
                      by = c("team_id", "game_id")) %>%
    # add backtoback calcs and team season game number
    dplyr::arrange(season_label, team_id, game_date) %>%
    dplyr::group_by(season_label, team_id) %>%
    dplyr::mutate(team_season_game_num = dplyr::row_number()) %>%
    dplyr::mutate(is_backtoback_front = 
                    dplyr::if_else(team_season_game_num == max(team_season_game_num) | dplyr::lead(game_date) != game_date + 1, 
                                   0, 1),
                  is_backtoback_back = 
                    dplyr::if_else(team_season_game_num == min(team_season_game_num) | dplyr::lag(game_date) != game_date - 1, 
                                   0, 1)) %>%
    dplyr::mutate(is_backtoback = 
                    dplyr::if_else(is_backtoback_front | is_backtoback_back, 1, 0)) %>%
    dplyr::ungroup() %>%
    # make a key to identify opponents, then tidy up
    dplyr::inner_join(boxscores %>% 
                        dplyr::group_by(game_id) %>% 
                        dplyr::summarise(team1 = dplyr::first(team_id),
                                         team2 = dplyr::last(team_id)) %>%
                        dplyr::ungroup(),
                      by = "game_id") %>%
    dplyr::mutate(opponent_team_id = dplyr::if_else(team_id == team1, team2, team1)) %>%
    dplyr::select(-dplyr::matches("team[12]"))
  # attach opponent stats
  message("adding opponent games...")
  team_games <- team_games %>%
    dplyr::inner_join(dplyr::select(team_games, -opponent_team_id, -game_date, -home_away),
                      by = c("season_label", "game_id", "opponent_team_id" = "team_id"),
                      suffix = c("", "_opp"))
  names(team_games) <- gsub("team_opp", "opp", names(team_games))
  # add outcome
  team_games <- team_games %>%
    dplyr::mutate(wl = dplyr::if_else(pts_team > pts_opp, "W", "L"),
                  margin = pts_team - pts_opp)
  # return output
  return(team_games)
}
