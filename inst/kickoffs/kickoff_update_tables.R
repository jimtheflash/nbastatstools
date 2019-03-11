message("kicking off basketball updates at ", Sys.time(), "...")
devtools::load_all("~/hlprs")
devtools::load_all("~/nbastatstools")
# db connection
db_connection <- hlprs::connect_to_postgres_db(.dbname = "basketball",
                                               .user = "postgres",
                                               .password = "R@sta!123")
# updates
nbastatstools::get_new_nba_boxscores(db_connection = db_connection,
                                     boxscores_table = "nba.boxscores",
                                     output_dir = "~/nba/boxscores")
# ingestion prep
message("kicking off basketball ingestion prep at ", Sys.time(), "...")
newest_boxscore <- hlprs::identify_newest_file(dir = "C:/Users/Jim/Documents/nba/boxscores")
hlprs::copy_file_and_rename(file_to_move = newest_boxscore,
                            new_dir = "C:/Users/Jim/Documents/nba/ingest",
                            new_name = "boxscores.csv")
# disconnect
RPostgreSQL::dbDisconnect(db_connection)