#' Enrich the bejesus out of nba team-game data
#' @importFrom magrittr %>%
#' @param team_games data.frame of nba team game data
#' @return a much, much wider data.frame of nba team game data
#' @export
enrich_nba_team_games <- function(team_games = NULL) {
  
  #### rolling stats require functions to be pre-specified ####
  ## TODO: add to hlprs, and breakout into separate list function and arg
  rollsum_five <- function(x) {zoo::rollsum(x, 5, na.pad = TRUE, align = "right")}
  rollsum_ten <- function(x) {zoo::rollsum(x, 10, na.pad = TRUE, align = "right")}
  rollsum_twenty <- function(x) {zoo::rollsum(x, 20, na.pad = TRUE, align = "right")}
  rollsd_five <- function(x) {
    suppressWarnings(zoo::rollapply(x, 5, sd, na.pad = TRUE, align = "right"))
  }
  rollsd_ten <- function(x) {
    suppressWarnings(zoo::rollapply(x, 10, sd, na.pad = TRUE, align = "right"))
  }
  rollsd_twenty <- function(x) {
    suppressWarnings(zoo::rollapply(x, 20, sd, na.pad = TRUE, align = "right"))
  }
  rollgamesaway_five <- function(x) {
    suppressWarnings(zoo::rollapply(x, 5, function(y) {sum(y == "away")}, na.pad = TRUE, align = "right"))
  }
  rollgamesaway_ten <- function(x) {
    suppressWarnings(zoo::rollapply(x, 10, function(y) {sum(y == "away")}, na.pad = TRUE, align = "right"))
  }
  rollgameswon_five <- function(x) {
    suppressWarnings(zoo::rollapply(x, 5, function(y) {sum(y == "win")}, na.pad = TRUE, align = "right"))
  }
  rollgameswon_ten <- function(x) {
    suppressWarnings(zoo::rollapply(x, 10, function(y) {sum(y == "win")}, na.pad = TRUE, align = "right"))
  }
  rollgameswon_twenty <- function(x) {
    suppressWarnings(zoo::rollapply(x, 20, function(y) {sum(y == "win")}, na.pad = TRUE, align = "right"))
  }
  cumulative_win_perc <- function(x) {dplyr::cummean(as.numeric(x == "win"))}
  
  #### add window functions ####
  message("adding team season window statistics...")
  enriched_team_games <- team_games %>%
    dplyr::arrange(team_id, season, season_game_num) %>%
    dplyr::group_by(team_id, season) %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::ends_with("_team")),
                     .funs = list(~rollsum_five,
                                  ~rollsum_ten,
                                  ~rollsum_twenty,
                                  ~rollsd_five,
                                  ~rollsd_ten,
                                  ~rollsd_twenty,
                                  ~cumsum,
                                  cummean = dplyr::cummean)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(home_away),
                     .funs = list(~rollgamesaway_five,
                                  ~rollgamesaway_ten))
  
  #### add lag statistics ####
  message("adding team season lag statistics...")
  enriched_team_games <- enriched_team_games %>%
    dplyr::mutate_at(.vars = dplyr::vars(home_away,
                                         dplyr::matches("roll"),
                                         dplyr::ends_with("_team")),
                     .funs = list(prev_game = dplyr::lag)) %>%
    dplyr::ungroup()
  
  #### add previous team-season statistics ####
  message("adding previous team-seaon statistics...")
  team_seasons <- team_games %>%
    dplyr::group_by(
      team_id,
      season) %>%
    dplyr::summarise_at(
      .vars = dplyr::vars(dplyr::ends_with("_team")),
      .funs = list(prev_season_total = sum,
                   prev_season_mean = mean)
    ) %>%
    dplyr::ungroup()
  enriched_team_games <- enriched_team_games %>%
    dplyr::mutate(season_lag = season - 1) %>%
    dplyr::left_join(team_seasons, by = c("team_id",
                                          "season_lag" = "season")) %>%
    dplyr::select(-season_lag)
  
  
  #### split into matchups of home and visitor ####
  message("splitting into matchup data...")
  # home data
  home_teams <- enriched_team_games %>% 
    dplyr::filter(home_away == "home") %>%
    dplyr::select(-home_away)
  
  home_identifiers <- home_teams %>%
    dplyr::select(game_id, game_date, season)
  
  home_to_rename <- home_teams %>%
    dplyr::select(-game_id, -game_date, -season)
  new_home_names <- paste0("home_", names(home_to_rename))
  names(home_to_rename) <- new_home_names
  
  home_to_join <- dplyr::bind_cols(home_identifiers, home_to_rename)
  
  # visitor data
  visitor_teams <- enriched_team_games %>%
    dplyr::filter(home_away == "away") %>%
    dplyr::select(-home_away)
  
  visitor_identifiers <- visitor_teams %>%
    dplyr::select(game_id, game_date, season)
  
  visitor_to_rename <- visitor_teams %>%
    dplyr::select(-game_id, -game_date, -season)
  new_visitor_names <- paste0("visitor_", names(visitor_to_rename))
  names(visitor_to_rename) <- new_visitor_names
  
  visitor_to_join <- dplyr::bind_cols(visitor_identifiers, visitor_to_rename)
  
  # join
  joined_team_games <- dplyr::inner_join(home_to_join, visitor_to_join,
                                         by = c("game_id", "game_date", "season"))
  
  #### engineer matchup stats ####
  message("engineering matchup-related stats...")
  joined_team_games <- joined_team_games %>%
    dplyr::mutate(total_pts = home_pts_team + visitor_pts_team) %>%
    dplyr::mutate(home_margin = home_pts_team - visitor_pts_team) %>%
    dplyr::mutate(home_win = dplyr::if_else(home_margin > 0, "win", "loss"))
  # extract season matchup stats in a loop
  sparse_team_games <- team_games %>%
    dplyr::select(team_id, game_id, game_date, season, home_away)
  margin <- list()
  wl <- list()
  for (i in 1:nrow(sparse_team_games)) {
    # get lookup row
    sparse_row <- sparse_team_games[i, ]
    target_game_id <- sparse_row$game_id
    # get data from joined_team_games
    home_margin <- joined_team_games$home_margin[joined_team_games$game_id == target_game_id]
    home_win <- joined_team_games$home_win[joined_team_games$game_id == target_game_id]
    # store in lists
    margin[[i]] <- home_margin
    wl[[i]] <- home_win
  }
  # add matchup stats to the sparse data, then apply window functions
  margin_num <- unlist(margin)
  sparse_team_games$margin <- ifelse(sparse_team_games$home_away == "home", 
                                     margin_num, margin_num * -1)
  wl_char <- unlist(wl)
  sparse_team_games$wl <- ifelse(sparse_team_games$home_away == "home", wl_char,
                                 ifelse(sparse_team_games$home_away == "away" & wl_char == "win", 
                                        "loss", "win"))
  sparse_team_games <- sparse_team_games %>%
    dplyr::arrange(team_id, season, game_date) %>%
    dplyr::group_by(team_id, season) %>%
    dplyr::mutate(margin_rollsum_five = rollsum_five(margin),
                  margin_rollsum_ten = rollsum_ten(margin),
                  margin_rollsum_twenty = rollsum_twenty(margin),
                  margin_rollsd_five = rollsd_five(margin),
                  margin_rollsd_ten = rollsd_ten(margin),
                  margin_rollsd_twenty = rollsd_twenty(margin),
                  rollgameswon_five = rollgameswon_five(wl),
                  rollgameswon_ten = rollgameswon_ten(wl),
                  rollgameswon_twenty = rollgameswon_twenty(wl),
                  cumulative_win_perc = cumulative_win_perc(wl))
  # calculate lags
  sparse_team_games <- sparse_team_games %>%
    dplyr::mutate_at(.vars = dplyr::vars(margin, wl, 
                                         dplyr::matches("cumulative"),
                                         dplyr::matches("roll")),
                     .funs = list(prev_game = dplyr::lag)) %>%
    dplyr::ungroup()
  # split into home and visitor as before, join to wide data
  ## home
  sparse_home_team_identifiers <- sparse_team_games %>%
    dplyr::filter(home_away == "home") %>%
    dplyr::select(game_id, game_date, season)
  sparse_home_to_rename <- sparse_team_games %>%
    dplyr::filter(home_away == "home") %>%
    dplyr::select(-game_id, -game_date, -season)
  names(sparse_home_to_rename) <- paste0("home_", names(sparse_home_to_rename))
  sparse_home_to_join <- dplyr::bind_cols(sparse_home_team_identifiers, 
                                          sparse_home_to_rename)
  joined_team_games <- dplyr::inner_join(joined_team_games, sparse_home_to_join)
  ## visitor
  sparse_visitor_team_identifiers <- sparse_team_games %>%
    dplyr::filter(home_away == "away") %>%
    dplyr::select(game_id, game_date, season)
  sparse_visitor_to_rename <- sparse_team_games %>%
    dplyr::filter(home_away == "away") %>%
    dplyr::select(-game_id, -game_date, -season)
  names(sparse_visitor_to_rename) <- paste0("visitor_", names(sparse_visitor_to_rename))
  sparse_visitor_to_join <- dplyr::bind_cols(sparse_visitor_team_identifiers, 
                                             sparse_visitor_to_rename)
  joined_team_games <- dplyr::inner_join(joined_team_games, sparse_visitor_to_join)
  #### add defensive stats ####
  message("adding defensive stats...")
  sparse_team_games <- team_games %>%
    dplyr::select(team_id, game_id, game_date, season, home_away, pts_team)
  pts_opp_list <- list()
  for (i in 1:nrow(sparse_team_games)) {
    pts_opp <- sparse_team_games$pts_team[sparse_team_games$game_id == sparse_team_games$game_id[[i]] &
                                            sparse_team_games$team_id != sparse_team_games$team_id[[i]]]
    pts_opp_list[[i]] <- pts_opp
  }
  pts_opp_num <- unlist(pts_opp_list)
  sparse_team_games$pts_allowed <- pts_opp_num
  # windows and lags
  sparse_team_games <- sparse_team_games %>%
    dplyr::arrange(team_id, season, game_date) %>%
    dplyr::group_by(team_id, season) %>%
    dplyr::mutate(pts_allowed_rollsum_five = rollsum_five(pts_allowed),
                  pts_allowed_rollsum_ten = rollsum_ten(pts_allowed),
                  pts_allowed_rollsum_twenty = rollsum_twenty(pts_allowed),
                  pts_allowed_rollsd_five = rollsd_five(pts_allowed),
                  pts_allowed_rollsd_ten = rollsd_ten(pts_allowed),
                  pts_allowed_rollsd_twenty = rollsd_twenty(pts_allowed)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("allowed")),
                     .funs = list(prev_game = dplyr::lag)) %>%
    dplyr::ungroup()
  
  # split into home/visitor and join
  sparse_home_team_identifiers <- sparse_team_games %>%
    dplyr::filter(home_away == "home") %>%
    dplyr::select(game_id, game_date, season)
  sparse_home_to_rename <- sparse_team_games %>%
    dplyr::filter(home_away == "home")
  names(sparse_home_to_rename) <- paste0("home_", names(sparse_home_to_rename))
  sparse_home_to_join <- dplyr::bind_cols(sparse_home_team_identifiers, 
                                          sparse_home_to_rename)
  joined_team_games <- dplyr::inner_join(joined_team_games, sparse_home_to_join)
  ## visitor
  sparse_visitor_team_identifiers <- sparse_team_games %>%
    dplyr::filter(home_away == "away") %>%
    dplyr::select(game_id, game_date, season)
  sparse_visitor_to_rename <- sparse_team_games %>%
    dplyr::filter(home_away == "away")
  names(sparse_visitor_to_rename) <- paste0("visitor_", names(sparse_visitor_to_rename))
  sparse_visitor_to_join <- dplyr::bind_cols(sparse_visitor_team_identifiers, 
                                             sparse_visitor_to_rename)
  joined_team_games <- dplyr::inner_join(joined_team_games, sparse_visitor_to_join)
  # return this very, very wide object
  return(joined_team_games)
}