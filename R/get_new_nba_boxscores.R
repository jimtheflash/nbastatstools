#' Get new boxscores from nba stats api
#' @importFrom magrittr %>%
#' @param db_connection RPostgreSQL connection object
#' @param boxscores_table character location of boxscores table in db
#' @param output_dir character path to output new boxscores
#' @return save a .csv to output_dir
#' @export
get_new_nba_boxscores <- function(db_connection = NULL,
                                  boxscores_table = NULL,
                                  output_dir = NULL) {
  # get maximum season and game_date from current boxscores
  lu <- DBI::dbGetQuery(db_connection, 
                        paste0("select max(season) AS max_season, ",
                               "max(game_date) AS max_game_date ",
                               "from ", boxscores_table))
  message("max_season = ", lu$max_season, "; max_game_date = ", lu$max_game_date)
  # get boxscores based on criteria in lu
  new_boxscores <- nbastatstools::get_nba_season_boxscores(season = lu$max_season) %>%
    nbastatstools::tidy_nba_boxscores() %>%
    dplyr::filter(game_date > lu$max_game_date)
  # don't write a file if there's nothing to write
  if (nrow(new_boxscores) == 0) {
    stop("no new games, no file will be written")
  }
  # if there is something to write, then save output
  output_path <- paste0(output_dir, "/boxscores_", Sys.Date(), ".csv")
  readr::write_csv(new_boxscores, path = output_path, na = "")
  message(nrow(new_boxscores), " observations saved to ", output_path)
}