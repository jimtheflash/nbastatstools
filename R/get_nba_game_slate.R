#' Get slate of nba games for a date
#' @importFrom magrittr %>%
#' @param game_date date or character formatted yyyy-mm-dd
#' @return slate of games for a date
#' @export
get_nba_game_slate <- function(game_date = Sys.Date()) {
  # build the url; uses the scoreboard endpoint
  scoreboard_url <- paste0("https://stats.nba.com/stats/scoreboard?",
                           "DayOffset=0&GameDate=", game_date,
                           "&LeagueID=00")
  # grab the json
  scoreboard <- curl::curl(scoreboard_url) %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = TRUE)
  # get GameHeader table for upcoming games
  gameheader_index <- which(scoreboard$resultSets$name == "GameHeader")
  game_header <- scoreboard$resultSets$rowSet[[gameheader_index]] %>%
    as.data.frame(., stringsAsFactors = FALSE)
  # return TRUE if no games
  if (length(game_header) == 0) {
    message("no games on ", game_date)
    return(TRUE)
  }
  # if there are games, assign names and return
  message(nrow(game_header), " games found on ", game_date)
  names(game_header) <- scoreboard$resultSets$headers[[gameheader_index]]
  return(game_header)
}

