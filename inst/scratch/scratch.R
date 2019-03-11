#### INITIAL DATA PULL ####
rm(list = ls())
t1 <- Sys.time()
devtools::load_all("~/hlprs")
devtools::load_all("~/nbastatstools")
# raw data
boxscores <- lapply(2000:2018,
                    function(x) {
                      message("getting boxscores for ", x)
                      nbastatstools::get_nba_season_boxscores(season = x)
                    }) %>%
  dplyr::bind_rows() %>%
  nbastatstools::tidy_nba_boxscores()
# pbp <-
#   nbastatstools::get_nba_playbyplay(gameids = unique(boxscores$game_id)) %>%
#   dplyr::bind_rows() %>%
#   nbastatstools::tidy_nba_playbyplay()
# player_profiles <-
#   nbastatstools::get_nba_player_profiles(playerids = unique(boxscores$player_id))
# 
# build tables from boxscores
team_games <- nbastatstools::make_nba_team_games(boxscores = boxscores)
# team_seasons <- nbastatstools::make_team_seasons()
#
# enrich
enriched_team_games <- nbastatstools::enrich_nba_team_games(team_games)
t2 <- Sys.time()
t2 - t1
# enriched_player_games <- nbastatstools::enrich_nba_player_games(boxscores)
#
#
# structure for modeling
scoped <- enriched_team_games %>%
  dplyr::filter(team_season_game_num >= 41 & team_season_game_num <= 82)

inputs_raw <- scoped %>%
  dplyr::select(game_id,
                team_id,
                opponent_team_id,
                dow,
                month,
                home_away,
                team_season_game_num,
                dplyr::starts_with("is_backtoback"),
                dplyr::ends_with("prev_game"))

opposing_team_inputs <- scoped %>%
  dplyr::select(game_id,
                opponent_team_id = team_id,
                team_season_game_num,
                dplyr::ends_with("prev_game"))
new_names <- ifelse(!grepl("_id", names(opposing_team_inputs)), 
                    paste0("opposing_team_", names(opposing_team_inputs)),
                    names(opposing_team_inputs))
names(opposing_team_inputs) <- new_names

fair_inputs <- inputs_raw %>%
  dplyr::left_join(opposing_team_inputs)

# recode non-numerics for input
non_numeric <- fair_inputs[, !sapply(fair_inputs, is.numeric)] %>%
  dplyr::select(-dplyr::ends_with("_id"))
dummy_obj <- caret::dummyVars(~., non_numeric)
library(caret)
dummied <- predict(dummy_obj, non_numeric) %>%
  as.data.frame() %>%
  dplyr::select(-dplyr::matches("Monday"),
                -dplyr::matches("October"),
                -dplyr::matches("awayaway"))
numeric <- fair_inputs[, sapply(fair_inputs, is.numeric)]
to_model <- data.frame(numeric, dummied) %>%
  dplyr::mutate(outcome = as.factor(scoped$wl)) %>%
  na.omit()
outcome <- to_model$outcome
to_model$outcome <- NULL
to_model <- as.matrix(to_model)
lasso <- glmnet::cv.glmnet(to_model, 
                           outcome, 
                           family = "binomial", 
                           nfolds = 100)
rf <- randomForest::randomForest(outcome ~ ., 
                                 data = data.frame(as.data.frame(to_model), outcome), 
                                 importance = TRUE)
