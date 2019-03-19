message(Sys.time(), " kicking off basketball updates...")
# db connection -----------------------------------------------------------
db_connection <- hlprs::connect_to_postgres_db(.dbname = "basketball",
                                               .user = "postgres",
                                               .password = "R@sta!123")
# updates -----------------------------------------------------------------
## boxscores
message(Sys.time(), " trying to get new boxscores...")
new_boxscores <- 
  nbastatstools::get_new_nba_boxscores(
    db_connection = db_connection,
    boxscores_table = "nba.boxscores",
    output_dir = "C:/Users/Jim/Documents/nba/boxscores",
    return_df = TRUE)
# ingestion prep ----------------------------------------------------------
if (nrow(new_boxscores) > 0) {
  message(Sys.time(), "kicking off basketball ingestion prep...")
  ## boxscores 
  newest_boxscores <- 
    hlprs::identify_newest_file(dir = "C:/Users/Jim/Documents/nba/boxscores")
  hlprs::copy_file_and_rename(file_to_move = newest_boxscores,
                              new_dir = "C:/Users/Jim/Documents/nba/ingest",
                              new_name = "boxscores.csv")
}
# db disconnect -----------------------------------------------------------
RPostgreSQL::dbDisconnect(db_connection)

