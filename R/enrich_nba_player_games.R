enrich_nba_player_games <- function(boxscores = NULL,
                                    enriched_team_games = NULL) {
  
  # calculate rolling stats requires functions
  ## TODO: add to hlprs, and breakout into separate list function and arg
  rollsum_five <- function(x) {zoo::rollsum(x, 5, na.pad = TRUE, align = "right")}
  rollsum_ten <- function(x) {zoo::rollsum(x, 10, na.pad = TRUE, align = "right")}
  rollsum_twenty <- function(x) {zoo::rollsum(x, 20, na.pad = TRUE, align = "right")}
  rollmean_five <- function(x) {zoo::rollmean(x, 5, na.pad = TRUE, align = "right")}
  rollmean_ten <- function(x) {zoo::rollmean(x, 10, na.pad = TRUE, align = "right")}
  rollmean_twenty <- function(x) {zoo::rollmean(x, 20, na.pad = TRUE, align = "right")}
  rollsd_ten <- function(x) {zoo::rollapply(x, 10, sd, na.pad = TRUE, align = "right")}
  rollsd_twenty <- function(x) {zoo::rollapply(x, 20, sd, na.pad = TRUE, align = "right")}
  
  t1 <- Sys.time()
  message("calculating game-level statistics...")
  enriched_player_games <- boxscores %>%
    dplyr::mutate(fg_perc = fgm / fga,
                  fgthree_perc = fgthreem / fgthreea,
                  ft_perc = ftm / fta) %>%
    # calculate four-factor stats
    ## TODO: get the other two factors
    dplyr::mutate(efg_perc = (fgm + (.5 * fgthreem)) / fga,
                  tov_perc = tov / (fga + tov + (0.44 * fta)))
  
  # calculate window stats
  message("calculating windowed statistics...")
  enriched_player_games <- enriched_player_games %>%
    dplyr::arrange(player_id, game_date) %>%
    dplyr::group_by(player_id, season_label) %>%
    dplyr::mutate_at(.vars = dplyr::vars(min,
                                         dplyr::starts_with("fg"),
                                         dplyr::starts_with("ft"),
                                         dplyr::ends_with("reb"),
                                         ast,
                                         stl,
                                         blk,
                                         tov,
                                         pf,
                                         pts,
                                         -dplyr::ends_with("perc")),
                     .funs = dplyr::funs(rollsum_five,
                                         rollsum_ten,
                                         rollsum_twenty,
                                         rollmean_five,
                                         rollmean_ten,
                                         rollmean_twenty,
                                         rollsd_ten,
                                         rollsd_twenty,
                                         cumsum))
  # calculate lag stats
  message("calculating lag statistics...")
  enriched_player_games <- enriched_player_games %>%
    dplyr::mutate_at(.vars = dplyr::vars(wl,
                                         home_away,
                                         game_date,
                                         dplyr::starts_with("min"),
                                         dplyr::starts_with("fg"),
                                         dplyr::starts_with("ft"),
                                         dplyr::starts_with("ast"),
                                         dplyr::starts_with("stl"),
                                         dplyr::starts_with("blk"),
                                         dplyr::starts_with("tov"),
                                         dplyr::starts_with("pf"),
                                         dplyr::starts_with("pts"),
                                         dplyr::matches("reb")),
                     .funs = dplyr::funs(prev_game = dplyr::lag)) %>%
    dplyr::ungroup()
  t2 <- Sys.time()
  t2-t1
  # return
  return(enriched_player_games)
}
