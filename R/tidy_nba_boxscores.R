#' tidy boxscores from nba stats api
#' @importFrom magrittr %>%
#' @param boxscores data.frame with boxscore data from nba stats api
#' @return tidy data.frame
#' @export
tidy_nba_boxscores <- function(boxscores = NULL) {
  # tidy names
  new_names <- names(boxscores) %>%
    tolower() %>%
    hlprs::convert_digits_to_words()
  names(boxscores) <- new_names
  # fix dates
  boxscores$game_date <- as.Date(boxscores$game_date)
  # remove pre-calculated columns (they're truncated at 2 digits)
  boxscores <- dplyr::select(boxscores, -dplyr::matches("_pct"))
  # convert non-id columns that should be numeric to numeric
  non_id_cols <- names(boxscores)[!grepl("_id", names(boxscores))]
  for (col in non_id_cols) {
    boxscores[[col]] <- hlprs::convert_char_to_numeric(boxscores[[col]])
  }
  return(boxscores)
}