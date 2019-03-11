#' Create nba.boxscores table in database
#' @importFrom magrittr %>%
#' @param seasons seasons to add for initial import
#' @param db_connection connection object created with \code{DBI::dbConnect()}
#' @param create_table_sql character path to sql file for creating nba.boxscores table
#' @return row count
#' @export
create_db_table_nba_boxscores <- function(seasons = NULL,
                                          db_connection = NULL,
                                          create_table_sql = NULL) {
  # get specified seasons, bind together, and tidy
  boxscores <- lapply(seasons,
                      function(x) {
                        message("getting boxscores for ", x, "...")
                        nbastatstools::get_nba_season_boxscores(season = x)
                      }) %>%
    dplyr::bind_rows() %>%
    nbastatstools::tidy_nba_boxscores()
  # create table
  message("creating nba.boxscores table...")
  DBI::dbGetQuery(db_connection, readr::read_file(create_table_sql))
  # insert into table
  message("appending boxscores into nba.boxscores table...")
  RPostgreSQL::dbWriteTable(db_connection, "nba.boxscores", boxscores, append = TRUE)
  # make sure there are rows there
  DBI::dbGetQuery(db_connection, "select count(*) AS obs FROM nba.boxscores;")
}