rm(list = ls())
t1 <- Sys.time()
devtools::load_all("~/nbastatstools")
# kickoff_model_building
message(Sys.time(), " kicking off model building...")
# config ------------------------------------------------------------------
# config <- yaml::read_yaml(commandArgs(trailingOnly = TRUE)[1])
config <- yaml::read_yaml("C:/Users/Jim/Documents/configs/update_nba_data.yaml")
# db connection -----------------------------------------------------------
db_connection <- hlprs::connect_to_postgres_db(.dbname = config$db$dbname,
                                               .user = config$db$user,
                                               .password = config$db$password)
# get data ----------------------------------------------------------------
message(Sys.time(), " getting data from db: ", config$db$dbname, "...")
boxscores <- 
  DBI::dbGetQuery(db_connection,
                  paste0("SELECT * FROM ", config$tables$boxscores_table, ";"))
team_games <- nbastatstools::make_nba_team_games(boxscores = boxscores)
# preprocess data ----------------------------------------------------------
## team-game data
message(Sys.time(), " preprocessing team-game data...")
enriched_team_games <- nbastatstools::enrich_nba_team_games(team_games)
preprocessed_team_games <- 
  nbastatstools::preprocess_nba_team_games(
    enriched_team_games,
    models = c("forty"),
    outcomes = c("home_win", "home_margin", "total_pts"),
    objects_output_path = "C:/Users/Jim/Documents/nba/objects")
## player-game data
# message(Sys.time(), " preprocessing player-game data...")
# enriched_player_games <- nbastatstools::enriche_nba_player_games(boxscores, team_games)
# build models ------------------------------------------------------------
## team-games
message(Sys.time(), " building team-games models...")
team_game_models <- 
  nbastatstools::build_nba_team_game_models(
    modeling_data = preprocessed_team_games$modeling_data,
    output_path = "C:/Users/Jim/Documents/nba/objects")
## games (forthcoming; requires team-games scores as inputs)
## player-games (forthcoming; requires team-games or games scores as inputs)
# evaluate models ---------------------------------------------------------
## (forthcoming)
# db disconnect -----------------------------------------------------------
RPostgreSQL::dbDisconnect(db_connection)
t2 <- Sys.time()
t2-t1

