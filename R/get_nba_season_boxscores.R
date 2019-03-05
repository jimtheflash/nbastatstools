#' get nba season boxscores
#' @importFrom magrittr %>%
#' @param season numeric season to get
#' @param season_types character vector, which part of the season to get; options include "Regular_Season" and "Playoffs"
#' @return data.frame of player-game boxscores
#' @export
get_nba_season_boxscores <- function(season = NULL,
                                     season_types = c("Regular_Season", "Playoffs")) {
  ## TODO: hardcoded params should be config'ed
  # build the season label as YYYY-YY
  season_label <- paste0(season, "-", substr(as.character(season + 1), 3, 4))
  # call api for each element of season_type
  season_boxscores <- list()
  for (season_type in season_types) {
    # encode the type
    type_label <- gsub("_", " ", season_type)
    type_label <- utils::URLencode(type_label)
    player_boxscore_url <- 
      paste0("https://stats.nba.com/stats/leaguegamelog?",
             "Counter=1000",
             "&Season=", season_label,
             "&Direction=DESC",
             "&LeagueID=00",
             "&PlayerOrTeam=P",
             "&SeasonType=", type_label,
             "&Sorter=DATE")
    # extract the data into a list from JSON
    boxscores_json <- player_boxscore_url %>%
      curl::curl() %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
    # build a data.frame
    boxscores_df <- as.data.frame(boxscores_json$resultSets$rowSet, stringsAsFactors = FALSE)
    # to next season_types element if this one is empty
    if (nrow(boxscores_df) == 0) {
      next
    }
    new_names <- unlist(boxscores_json$resultSets$headers)
    names(boxscores_df) <- new_names
    boxscores_df$season_type <- season_type
    # store data
    season_boxscores[[season_type]] <- boxscores_df
  }
  # return the output as a data.frame
  season_boxscores_df <- dplyr::bind_rows(season_boxscores)
  return(season_boxscores_df)
}
