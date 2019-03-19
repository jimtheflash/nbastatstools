rm(list = ls())
t1 <- Sys.time()
devtools::load_all("~/nbastatstools")
# kickoff_model_building
message(Sys.time(), " kicking off scoring games...")
# config ------------------------------------------------------------------
# config <- yaml::read_yaml(commandArgs(trailingOnly = TRUE)[1])
config <- yaml::read_yaml("C:/Users/Jim/Documents/configs/update_nba_data.yaml")
# db connection -----------------------------------------------------------
db_connection <- hlprs::connect_to_postgres_db(.dbname = config$db$dbname,
                                               .user = config$db$user,
                                               .password = config$db$password)
# get games to score ------------------------------------------------------
game_slate <- nbastatstools::get_nba_game_slate() %>%
  nbastatstools::tidy_nba_game_slate()
# get data ----------------------------------------------------------------
message(Sys.time(), " getting data from db: ", config$db$dbname, "...")
boxscores_for_scoring <- 
  DBI::dbGetQuery(db_connection,
                  paste0("select * ",
                         "from ", config$tables$boxscores_table,
                         " where ",
                         "season >= (select max(season) ",
                         "from ", config$tables$boxscores_table, ") - 1;"))
team_games_for_scoring <- 
  nbastatstools::make_nba_team_games(boxscores = boxscores_for_scoring)
# prepare data for scoring -----------------------------------------------
## team-game data
message(Sys.time(), " preparing team-game data for scoring...")
enriched_team_games_for_scoring <- 
  nbastatstools::enrich_nba_team_games_for_scoring(
    game_slate = game_slate, 
    team_games_for_scoring = team_games_for_scoring
  )
preprocessed_team_games_for_scoring <- 
  nbastatstools::preprocess_nba_team_games_for_scoring(
    enriched_team_games_for_scoring = enriched_team_games_for_scoring,
    modeling_objects_path = 
      "C:/Users/Jim/Documents/nba/objects"
    )
# generate scores ---------------------------------------------------------
message(Sys.time(), " generating scores...")
scored_team_games <- nbastatstools::score_nba_team_games(
  model_inputs = preprocessed_team_games_for_scoring,
  model_path = "C:/Users/Jim/Documents/nba/objects"
  )

t2 <- Sys.time()
t2 - t1