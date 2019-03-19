message(Sys.time(), " kicking off basketball updates...")
# config ------------------------------------------------------------------
config <- yaml::read_yaml(commandArgs(trailingOnly = TRUE)[1])
# db connection -----------------------------------------------------------
db_connection <- hlprs::connect_to_postgres_db(.dbname = config$db$dbname,
                                               .user = config$db$user,
                                               .password = config$db$password)
# updates -----------------------------------------------------------------
message(Sys.time(), " trying to get new boxscores...")
new_boxscores <- 
  nbastatstools::get_new_nba_boxscores(db_connection = db_connection,
                                       boxscores_table = config$tables$boxscores_table,
                                       output_dir = config$paths$boxscores_path,
                                       return_df = TRUE)
if (nrow(new_boxscores) > 0) {
  message(Sys.time(), " making new team_games...")
  new_team_games <- 
    nbastatstools::make_nba_team_games(boxscores = new_boxscores,
                                       output_dir = config$paths$team_games_path,
                                       return_df = TRUE)
}
# ingestion prep ----------------------------------------------------------
if (nrow(new_boxscores) > 0) {
  message(Sys.time(), "kicking off basketball ingestion prep...")
  # boxscores 
  newest_boxscores <- hlprs::identify_newest_file(dir = config$paths$boxscores_path)
  hlprs::copy_file_and_rename(file_to_move = newest_boxscores,
                              new_dir = config$paths$ingestion_path,
                              new_name = config$filenames$boxscores)
  # team_games
  newest_team_games <- hlprs::identify_newest_file(dir = config$paths$boxscores_path)
  hlprs::copy_file_and_rename(file_to_move = newest_team_games,
                              new_dir = config$paths$ingestion_path,
                              new_name = config$filenames$team_games)
}
# db disconnect -----------------------------------------------------------
RPostgreSQL::dbDisconnect(db_connection)

