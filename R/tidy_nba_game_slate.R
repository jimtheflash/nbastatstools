#' Tidy game_slate
#' @param game_slate data.frame output of \code{nbastatstools::get_nba_game_slate()}
#' @return data.frame
#' @export
tidy_nba_game_slate <- function(game_slate = NULL) {
  # lower column names
  new_names <- tolower(names(game_slate))
  names(game_slate) <- new_names
  # select identifiers and date
  tidy_slate <- dplyr::transmute(game_slate,
                                 game_id,
                                 game_date = as.Date(game_date_est),
                                 home_team_id,
                                 visitor_team_id)
  # return output
  return(tidy_slate)
}