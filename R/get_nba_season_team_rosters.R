#' Query the commonteamroster endpoint from nba stats
#' @param seasons numeric seasons to grab (e.g. 2000)
#' @param teamids character that looks like a numeric but could have leading zeroes, teamids to grab, usually the TEAM_ID field in a boxscore
#' @param sleep logical sleep between API calls
#' @return list of team rosters; each entry will have players and coaches if available
#' @export
get_nba_season_team_rosters <- function(seasons = NULL,
                                        teamids = NULL,
                                        sleep = TRUE) {
  # make a grid of team seasons
  team_seasons <- expand.grid(season = seasons,
                              team_id = teamids, 
                              stringsAsFactors = FALSE)
  team_seasons$season_id <- 
    as.character(paste0(seasons, "-", substring(as.character(seasons + 1), 3)))
  team_rosters <- list()
  for (row in 1:nrow(team_seasons)) {
    # get values in row for url
    team_season <- dplyr::filter(team_seasons, 
                                 dplyr::row_number() == row)
    team_id <- team_season$team_id
    season_id <- team_season$season_id
    # build url
    team_roster_url <- paste0("https://stats.nba.com/stats/commonteamroster?",
                              "LeagueID=00",
                              "&Season=", season_id,
                              "&TeamID=", team_id)
    # get team roster
    team_roster_json <- curl::curl(team_roster_url) %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
    # structure as players and coaches dfs
    #players
    players_df <- team_roster_json$resultSets$rowSet[[1]] %>%
      as.data.frame(stringsAsFactors = FALSE)
    if (length(players_df) > 0) {
      names(players_df) <- team_roster_json$resultSets$headers[[1]]
    }
    # coaches
    coaches_df <- team_roster_json$resultSets$rowSet[[2]] %>%
      as.data.frame(stringsAsFactors = FALSE)
    if (length(coaches_df) > 0) {
      names(coaches_df) <- team_roster_json$resultSets$headers[[2]]
    }
    # store in a list
    message("storing data for ", team_id, " for ", season_id)
    team_rosters[[row]] <- list()
    team_rosters[[row]]$players <- players_df
    team_rosters[[row]]$coaches <- coaches_df
    # sleep if specified
    if (sleep == TRUE) {
      Sys.sleep(runif(1, 1, 3))
    }
  }
  return(team_rosters)
}