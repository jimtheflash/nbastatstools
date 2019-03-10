#' tidy playbyplay from nba stats api
#' @param playbyplay data.frame with playbyplay data from nba stats api
#' @return tidy data.frame
#' @export
tidy_nba_playbyplay <- function(playbyplay = NULL) {
  # tidy names
  new_names <- tolower(names(playbyplay))
  names(playbyplay) <- new_names
  # fix time columns
  playbyplay$wctime <- hms::parse_hm(playbyplay$wctimestring)
  playbyplay$wctimestring <- NULL
  period_secs <- strsplit(playbyplay$pctimestring, ":")
  mins <- as.numeric(unlist(lapply(period_secs, "[[", 1)))
  secs <- as.numeric(unlist(lapply(period_secs, "[[", 2)))
  playbyplay$pctime_secs <- (mins * 60) + secs
  playbyplay$pctimestring <- NULL
  # convert non-id non-time columns that should be numeric to numeric
  non_id_cols <- names(playbyplay)[!grepl("_id|time", names(playbyplay))]
  for (col in non_id_cols) {
    playbyplay[[col]] <- hlprs::convert_char_to_numeric(playbyplay[[col]])
  }
  return(playbyplay)
}
