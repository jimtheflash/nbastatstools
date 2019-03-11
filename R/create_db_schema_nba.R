#' Create nba schema in database
#' @param db_connection connection object created with \code{DBI::dbConnect()}
#' @param create_schema_sql character path to sql file for creating nba schema
#' @return nothing
#' @export
create_db_schema_nba <- function(db_connection = NULL,
                                 create_schema_sql = NULL) {
  # create schema
  message("creating nba schema...")
  DBI::dbGetQuery(db_connection, readr::read_file(create_schema_sql))
}
