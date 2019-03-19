#' Build NBA team-game models
#' @param modeling_data output of \code{preprocess_nba_team_games()}
#' @param output_path character path where models should be saved
#' @param n_cores number of cores to use for parallelization
#' @return a list of models, an ensemble, and data
#' @export
build_nba_team_game_models <- function(modeling_data = NULL,
                                       output_path = NULL,
                                       n_cores = round(parallel::detectCores() / 2)) {
  # load caret library
  library(caret)
  # loop through model names from modeling_data to build models for datat that exist
  models <- list()
  model_names <- names(modeling_data)
  for (model in model_names) {
    message("building '", model, "' models...")
    target_list <- list()
    # subset the modeling_data to include data and outcomes for current model
    subset_modeling_data <- modeling_data[[model]]
    modeling_input <- subset_modeling_data$inputs
    modeling_targets <- subset_modeling_data$targets
    
    # loop through outcomes and build models; train based on outcome class
    target_names <- names(modeling_targets)
    target_list <- list()
    for (target in target_names) {
      message("modeling ", target, "...")
      # set outcome variable
      outcome <- modeling_targets[[target]]
      # specify trainControl to be used in modeling
      if (!is.numeric(outcome)) {
        train_control <- caret::trainControl(method = "cv", number = 5, # TODO: MAKE ARG IN CONFIG
                                             search = "grid",
                                             savePredictions = "final",
                                             returnResamp = "final",
                                             classProbs = TRUE,
                                             allowParallel = TRUE) 
      } else {
        train_control <- caret::trainControl(method = "cv", number = 5, # TODO: MAKE ARG IN CONFIG
                                             search = "grid",
                                             savePredictions = "final",
                                             returnResamp = "final",
                                             allowParallel = TRUE)
      }
      # split based on outcome for assessment
      dp <- caret::createDataPartition(outcome, p = .75, list = FALSE)
      training_input <- modeling_input[dp, ]
      training_outcome <- outcome[dp]
      
      # build models
      cl <- parallel::makeCluster(n_cores)
      doParallel::registerDoParallel(cl)
      model_list <- caretEnsemble::caretList(
        training_input,
        training_outcome,
        trControl = train_control,
        methodList = c("gbm",
                       "glmnet"),
        tuneList = list(
          rf = caretEnsemble::caretModelSpec(method = "rf", 
                                             tuneGrid = data.frame(.mtry = 10)),
          earth = caretEnsemble::caretModelSpec(method = "earth",
                                                tuneGrid = data.frame(.nprune = 10,
                                                                      .degree = 2))
        ),
        continue_on_fail = FALSE)
      ensemble <- caretEnsemble::caretEnsemble(model_list)
      # create evaluation data
      test_input <- modeling_input[-dp, ]
      test_outcome <- outcome[-dp]
      # generate scores based on outcome type
      if (!is.numeric(outcome)) {
        test_preds <- predict(ensemble, test_input, type = "prob")
      } else {
        test_preds <- predict(ensemble, test_input)
      }
      # make output list
      output_list <-list(target = target,
                         ensemble = ensemble,
                         test_data = list(
                           test_preds = test_preds,
                           test_outcome = test_outcome)
                         )
      target_list[[target]] <- output_list
      # un-register parallel
      foreach::registerDoSEQ()                        
      parallel::stopCluster(cl)
    }
    models[[model]] <- target_list
    # save the models to the output_path if it isn't NULL
    if (!is.null(output_path)) {
      rds_name <- paste0(output_path, "/", model, "_models_",
                         gsub("\\.", "_", as.numeric(Sys.time())),
                         ".rds")
      saveRDS(target_list, file = rds_name)
    }
  }
  # unload caret so it doesn't cause other problems downstream
  detach("package:caret", unload = TRUE)
  # return model objects
  return(models)
}
