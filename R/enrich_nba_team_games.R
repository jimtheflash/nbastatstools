#' Enrich the bejesus out of nba team-game data
#' @importFrom magrittr %>%
#' @param team_games data.frame of nba team game data
#' @return a much, much wider data.frame of nba team game data
#' @export
enrich_nba_team_games <- function(team_games = NULL) {
  
  # rolling stats require functions to be pre-specified
  ## TODO: add to hlprs, and breakout into separate list function and arg
  rollsum_five <- function(x) {zoo::rollsum(x, 5, na.pad = TRUE, align = "right")}
  rollsum_ten <- function(x) {zoo::rollsum(x, 10, na.pad = TRUE, align = "right")}
  rollsum_twenty <- function(x) {zoo::rollsum(x, 20, na.pad = TRUE, align = "right")}
  rollsd_ten <- function(x) {zoo::rollapply(x, 10, sd, na.pad = TRUE, align = "right")}
  rollsd_twenty <- function(x) {zoo::rollapply(x, 20, sd, na.pad = TRUE, align = "right")}
  rollgamesaway_five <- function(x) {zoo::rollapply(x, 5, function(y) {sum(y == "away")}, na.pad = TRUE, align = "right")}
  rollgamesaway_ten <- function(x) {zoo::rollapply(x, 10, function(y) {sum(y == "away")}, na.pad = TRUE, align = "right")}
  rollgameswon_five <- function(x) {zoo::rollapply(x, 5, function(y) {sum(y == "W")}, na.pad = TRUE, align = "right")}
  rollgameswon_ten <- function(x) {zoo::rollapply(x, 10, function(y) {sum(y == "W")}, na.pad = TRUE, align = "right")}
  # add day of week, month of year
  enriched_team_games <- team_games %>%
    dplyr::mutate(dow = weekdays(game_date),
                  month = months(game_date))
  # add window functions
  message("adding team season window statistics...")
  enriched_team_games <- enriched_team_games %>%
    dplyr::arrange(team_id, season_label, team_season_game_num) %>%
    dplyr::group_by(team_id, season_label) %>%
    dplyr::mutate_at(.vars = dplyr::vars(margin,
                                         dplyr::ends_with("_team"),
                                         dplyr::ends_with("_opp"),
                                         -dplyr::starts_with("min"),
                                         -dplyr::starts_with("is_back"),
                                         -dplyr::starts_with("players_used")),
                     .funs = dplyr::funs(rollsum_five,
                                         rollsum_ten,
                                         rollsum_twenty,
                                         rollsd_ten,
                                         rollsd_twenty,
                                         cumsum,
                                         cummean = dplyr::cummean)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(home_away),
                     .funs = dplyr::funs(rollgamesaway_five,
                                         rollgamesaway_ten)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(wl),
                     .funs = dplyr::funs(rollgameswon_five,
                                         rollgameswon_ten))
  # add lag statistics
  message("adding team season lag statistics...")
  enriched_team_games <- enriched_team_games %>%
    dplyr::mutate_at(.vars = dplyr::vars(margin,
                                         wl,
                                         home_away,
                                         dplyr::matches("roll"),
                                         dplyr::ends_with("_team"),
                                         dplyr::ends_with("_opp"),
                                         -dplyr::starts_with("min"),
                                         -dplyr::starts_with("is_back")),
                     .funs = dplyr::funs(prev_game = dplyr::lag)) %>%
    dplyr::ungroup()
  # add previous team-season statistics
  message("adding previous team-seaon statistics...")
  team_seasons <- team_games %>%
    dplyr::group_by(
      team_id,
      season,
    ) %>%
    dplyr::summarise_at(
      .vars = dplyr::vars(dplyr::ends_with("_team"),
                          dplyr::ends_with("_opp"),
                          dplyr::matches("margin"),
                          -dplyr::starts_with("min"),
                          -dplyr::matches("backtoback"),
                          -dplyr::matches("season_game_num")),
      .funs = list(prev_season_total = ~sum(.),
                   prev_season_mean = ~mean(.))
    ) %>%
    dplyr::ungroup()
  enriched_team_games <- enriched_team_games %>%
    dplyr::mutate(season_lag = season - 1) %>%
    dplyr::left_join(team_seasons, by = c("team_id",
                                          "season_lag" = "season")) %>%
    dplyr::select(-season_lag)
  # add matchup opponent data
  message("adding opposing team data...")
  opposing_team_inputs <- enriched_team_games %>%
    dplyr::select(game_id,
                  team_id,
                  opponent_team_id,
                  team_season_game_num,
                  dplyr::ends_with("prev_game"),
                  dplyr::matches("prev_season")) %>%
    dplyr::mutate(opponent_team_id = team_id) %>%
    dplyr::select(-team_id)
  new_names <- ifelse(!grepl("_id", names(opposing_team_inputs)), 
                      paste0("opposing_team_", names(opposing_team_inputs)),
                      names(opposing_team_inputs))
  names(opposing_team_inputs) <- new_names
  enriched_team_games <- enriched_team_games %>%
    dplyr::left_join(opposing_team_inputs, by = c("game_id", "opponent_team_id"))
  # convert back into a data.frame and return
  enriched_team_games <- as.data.frame(enriched_team_games, stringsAsFactors = FALSE)
  return(enriched_team_games)
}