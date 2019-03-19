#' Generate NBA team-game scores using selected models
#' @importFrom magrittr %>%
#' @param preprocessed_team_games_for_scoring list of data.frames as model inputs
#' @param model_path character string path to model objects
#' @return list of data.frames with model outputs
#' @export
score_nba_team_games <- function(preprocessed_team_games_for_scoring = NULL,
                                 model_path = NULL) {
  # load caret library
  library(caret)
  # loop through the input data, load appropriate models, score games
  model_names <- names(preprocessed_team_games_for_scoring)
  output_list <- list()
  for (model_name in model_names) {
    # subset the input data
    model_input <- preprocessed_team_games_for_scoring[[model_name]]
    # move on if no data to score
    if (!nrow(model_input) >= 1) {
      message("no data for ", model_name, " model, moving to next model...")
      next
    }
    # get the latest models for that subset data
    newest_models_path <- hlprs::identify_newest_file(
      dir = model_path,
      pattern = paste0(model_name, "_models"))
    newest_models <- readRDS(newest_models_path)
    # loop through targets and output predictions
    target_list <- names(newest_models)
    target_model_output <- list()
    for (target in target_list) {
      target_ensemble <- newest_models[[target]]$ensemble
      model_type <- target_ensemble$ens_model$modelType
      if (model_type == "Regression") {
        preds <- predict(target_ensemble, newdata = model_input, 
                         unkOnly = TRUE)
      } else {
        preds <- predict(target_ensemble, newdata = model_input, 
                         unkOnly = TRUE, type = "prob")
      }
      # label the preds with identifiers
      preds_df <- model_input %>%
        dplyr::select(team_id, team_abbreviation) %>%
        dplyr::mutate(target = target,
                      preds = preds,
                      opp_team_id = model_input$opposing_team_team_id,
                      opp_team_abbreviation = 
                        model_input$opposing_team_team_abbreviation)
      target_model_output[[target]] <- preds_df
    }
    # add the target outputs to the final output list
    output_list[[model_name]] <- target_model_output
  }
  # unload caret so it doesn't cause other problems downstream
  detach("package:caret", unload = TRUE)
  # return list of predictions
  return(output_list)
}