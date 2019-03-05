get_nba_season_team_rosters <- function(boxscores = NULL) {
  # handle errors
  if (!is.data.frame(boxscores)) {
    stop("boxscores needs to be a data.frame")
  }
  if (sum(c("TEAM_ID", "SEASON_ID") %in% names(boxscores)) != 2) {
    stop("missing either one of critical columns TEAM_ID or SEASON_ID")
  }
  # get team seasons for roster url's
  team_seasons<- boxscores %>%
    dplyr::group_by(TEAM_ID, SEASON_ID) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()
  # loop through the team seasons and grab rosters
  team_rosters <- list()
  for (row in 1:nrow(team_seasons)) {
    # get values in row for url
    team_season <- dplyr::filter(team_seasons, dplyr::row_number() == row)
    team_id <- team_season$TEAM_ID
    season_num <- as.numeric(substring(team_season$SEASON_ID, 2))
    season_id <- paste0(season_num, "-", substring(as.character(season_num + 1), 3))
    # build url
    team_roster_url <- paste0("https://stats.nba.com/stats/commonteamroster?",
                              "LeagueID=",
                              "&Season=", season_id,
                              "&TeamID=", team_id)
    # get team roster
    team_roster_json <- curl::curl(team_roster_url) %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
    
  }
}