#' get player profiles from nba stats api
#' @importFrom magrittr %>%
#' @param playerids character vector of playerids
#' @param sleep logical should there be \code{Sys.sleep()} between playerids; default is \code{TRUE}
#' @return data.frame of player profiles
#' @export
get_nba_player_profiles <- function(playerids = NULL,
                                    sleep = TRUE) {
  output <- list()
  for (playerid in playerids) {
    message("grabbing info for playerid ", playerid)
    player_url <- paste0("https://stats.nba.com/stats/",
                         "commonplayerinfo?",
                         "LeagueID=&PlayerID=", playerid)
    profile_json <- curl::curl(player_url) %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
    profile_df <- as.data.frame(profile_json$resultSets$rowSet[[1]], 
                                stringsAsFactors = FALSE)
    new_names <- profile_json$resultSets$headers[[1]]
    names(profile_df) <- new_names
    output[[length(output) + 1]] <- profile_df
    if (sleep == TRUE) {
      Sys.sleep(runif(1, 2, 5))
    }
  }
  output_df <- dplyr::bind_rows(output)
  return(output_df)
}