#' Apply preprocessing and dummy-variable coding in prep for scoring
#' @param enriched_team_games_for_scoring data.frame of of nba team-game data 
#' @param modeling_objects_path path to object that contains modeling objects
#' @return list of data for modeling
#' @export
preprocess_nba_team_games_for_scoring <- function(enriched_team_games_for_scoring = NULL,
                                                  modeling_objects_path = NULL) {
  # load modeling objects
  newest_modeling_objects_path <- hlprs::identify_newest_file(
    dir = modeling_objects_path,
    pattern = "modeling_objects")
  modeling_objects <- readRDS(newest_modeling_objects_path)
  # load caret
  library(caret)
  # loop through each model preproc object and output data into a list
  model_names <- names(modeling_objects)
  output_list <- list()
  for (model_name in model_names) {
    # get objects
    preproc <- modeling_objects[[model_name]]$preproc
    dummyvars <- modeling_objects[[model_name]]$dummy
    # apply preproc
    nums_to_preproc <- 
      enriched_team_games_for_scoring[, names(enriched_team_games_for_scoring) %in% 
                                        names(preproc$mean)]
    preprocessed_nums <- predict(preproc, newdata = nums_to_preproc)
    # apply dummies
    factors_to_dummy <-
      enriched_team_games_for_scoring[, names(enriched_team_games_for_scoring) %in% 
                                        dummyvars$vars]
    factors_to_dummy <- dplyr::mutate_if(factors_to_dummy, is.character, as.factor)
    ## adjust levels
    for (column_name in names(factors_to_dummy)) {
      levels(factors_to_dummy[[column_name]]) <- dummyvars$lvls[[column_name]]
    }
    dummyvared_factors <- as.data.frame(predict(dummyvars, factors_to_dummy))
    names(dummyvared_factors) <- gsub("\\.", "_", names(dummyvared_factors))
    # combine
    combined_preproc_dummies <- dplyr::bind_cols(
      enriched_team_games_for_scoring[, 
                                      !names(enriched_team_games_for_scoring) %in% 
                                        c(names(nums_to_preproc),
                                          names(factors_to_dummy))],
                                                 preprocessed_nums,
                                                 dummyvared_factors)
    # filter based on game numbers
    if (model_name == "first") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num == 1)
    }
    if (model_name == "two") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 1,
                                 team_season_game_num <= 5)
    }
    if (model_name == "five") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 5,
                                 team_season_game_num <= 10)
    }
    if (model_name == "ten") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 10,
                                 team_season_game_num <= 20)
    }
    if (model_name == "twenty") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 20,
                                 team_season_game_num <= 40)
    }
    if (model_name == "forty") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 40,
                                 team_season_game_num <= 60)
    }
    if (model_name == "sixty") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 60,
                                 team_season_game_num <= 82)
    }
    if (model_name == "playoffs") {
      output_df <- dplyr::filter(combined_preproc_dummies,
                                 team_season_game_num > 82)
    }
    # store the data
    output_list[[model_name]] <- output_df
  }
  # return list of data.frames
  return(output_list)
}