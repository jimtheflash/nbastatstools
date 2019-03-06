#' get play-by-play for games from nba stats api
#' @importFrom magrittr %>%
#' @param gameid character string usually represented in data as \code{GAME_ID}
#' @return a list of data.frames
#' @export
get_nba_playbyplay <- function(gameids = NULL,
                               sleep = TRUE) {
  # loop through gameid's and grab playbyplay data
  pbp_list <- list()
  for (gameid in gameids) {
    # build url to scrape
    pbp_url <- paste0("https://stats.nba.com/stats/playbyplay?",
                      "EndPeriod=0",
                      "&GameID=", gameid, 
                      "&StartPeriod=0")
    # curl it
    pbp <- curl::curl(pbp_url) %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
    # turn to df
    pbp_df <- pbp$resultSets$rowSet[[1]] %>%
      as.data.frame(stringsAsFactors = FALSE)
    names(pbp_df) <- pbp$resultSets$headers[[1]]
    # verbose
    message("storing play-by-play data for gameid ", gameid)
    pbp_list[[gameid]] <- pbp_df
    # sleep if specified
    if (sleep == TRUE) {
      Sys.sleep(runif(1, 1, 3))
    }
  }
  # return the list
  return(pbp_list)
}
