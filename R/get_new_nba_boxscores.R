#' Get new boxscores from nba stats api
#' @importFrom magrittr %>%
#' @param db_connection RPostgreSQL connection object
#' @param boxscores_table character location of boxscores table in db
#' @param output_dir character path to output new boxscores
#' @return save a .csv to output_dir
#' @export
get_new_nba_boxscores <- function(db_connection = NULL,
                                  boxscores_table = NULL,
                                  output_dir = NULL,
                                  return_df = TRUE) {
  # get maximum season from current boxscores
  season_lu <- DBI::dbGetQuery(db_connection, 
                               paste0("select max(season) AS max_season ",
                               "from ", boxscores_table))
  max_season <- max(season_lu$max_season)
  # get list of game_ids from current season
  game_ids <- DBI::dbGetQuery(db_connection,
                              paste0("select distinct game_id ",
                                     "from ", boxscores_table, " ",
                                     "where season = ", max_season, ";"))
  # get boxscores based on criteria in lu
  new_boxscores <- nbastatstools::get_nba_season_boxscores(season = max_season) %>%
    nbastatstools::tidy_nba_boxscores() %>%
    dplyr::filter(!game_id %in% unique(game_ids$game_id))
  # don't write a file if there's nothing to write
  if (nrow(new_boxscores) == 0) {
    stop("no new games, no file will be written")
  }
  # if there is something to write, then save output
  output_path <- paste0(output_dir, "/boxscores_", Sys.Date(), ".csv")
  readr::write_csv(new_boxscores, path = output_path, na = "")
  message(nrow(new_boxscores), " observations saved to ", output_path)
  # if specified, return a data.frame
  if (return_df == TRUE) {
    return(new_boxscores)
  }
}