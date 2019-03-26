#' make a team-games table from nba stats api boxscores
#' @importFrom magrittr %>%
#' @param boxscores data.frame of boxscores from nba stats api
#' @return data.frame of team_game info for easy joining
#' @export
make_nba_team_games <- function(boxscores = NULL) {
  # make table --------------------------------------------------------------
  message("building team games from boxscores...")
  team_games <- boxscores %>%
    dplyr::group_by(
      team_id,
      game_id,
      game_date,
      season,
      home_away
    ) %>%
    dplyr::summarise_at(
      .vars = dplyr::vars(
        min,
        dplyr::starts_with("fg"),
        dplyr::starts_with("ft"),
        dplyr::ends_with("reb"),
        ast,
        stl,
        blk,
        tov,
        pf,
        pts
      ),
      .funs = list(team = ~sum(.))
    ) %>%
    dplyr::ungroup() %>%
    # add additional summary measures
    dplyr::inner_join(
      boxscores %>%
        dplyr::group_by(team_id, game_id) %>%
        dplyr::summarise(players_used = dplyr::n_distinct(player_id)) %>%
        dplyr::ungroup(),
      by = c("team_id", "game_id")
    ) %>%
    # add backtoback calcs and team season game number
    dplyr::arrange(season, team_id, game_date) %>%
    dplyr::group_by(season, team_id) %>%
    dplyr::mutate(season_game_num = dplyr::row_number()) %>%
    dplyr::mutate(
      is_backtoback_front =
        dplyr::if_else(
          season_game_num == max(season_game_num) |
            dplyr::lead(game_date) != game_date + 1,
          0, 1
        ),
      is_backtoback_back =
        dplyr::if_else(
          season_game_num == min(season_game_num) |
            dplyr::lag(game_date) != game_date - 1,
          0, 1
        )
    ) %>%
    dplyr::mutate(is_backtoback =
                    dplyr::if_else(is_backtoback_front |
                                     is_backtoback_back, 
                                   1, 0
                    )) %>%
    dplyr::ungroup()
  # return output -------------------------------------------------------------
  return(team_games)
}
