#' Preprocess team-game data for model-building and scoring
#' @importFrom magrittr %>%
#' @param enriched_team_games data.frame with all features engineered
#' @param models models to build
#' @param outcomes outcomes to predict for the models to build
#' @return list of pre-processing objects and data for building models
#' @export
preprocess_nba_team_games <- function(enriched_team_games = NULL,
                                      models = NULL,
                                      outcomes = NULL,
                                      objects_output_path = NULL) {
  # load caret else it's a jerk
  library(caret)
  # global filters
  raw <- enriched_team_games %>%
    dplyr::filter(season > min(season)) %>%
    dplyr::select(-dplyr::ends_with("cummean"),
                  -dplyr::ends_with("cumsum"),
                  -dplyr::ends_with("five"),
                  -dplyr::ends_with("ten"),
                  -dplyr::ends_with("twenty"))
  # apply preproc by model-subset
  # save output data and preproc objects separately for easier usage
  modeling_objects <- list()
  modeling_data <- list()
  for (model in models) {
    message("filtering data for model ", model, "...")
    if (model == "one") {
      # select inputs and outcomes
      filtered <- raw %>%
        dplyr::filter(home_season_game_num == 1 | visitor_season_game_num == 1) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "two") {
      # select inputs and outcomes
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 1 & 
                        visitor_season_game_num > 1 &
                        (home_season_game_num <= 5 | visitor_season_game_num <= 5)) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      -dplyr::matches("roll"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "five") {
      # select inputs and outcomes
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 5 &
                        visitor_season_game_num > 5 &
                        (home_season_game_num <= 10 | visitor_season_game_num <= 10)) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      dplyr::matches("backtoback"),
                      dplyr::matches("roll"),
                      -dplyr::matches("ten"),
                      -dplyr::matches("twenty"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "ten") {
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 10 &
                        visitor_season_game_num > 10 &
                        (home_season_game_num <= 20 | visitor_season_game_num <= 20)) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      dplyr::matches("backtoback"),
                      dplyr::matches("roll"),
                      -dplyr::matches("twenty"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "twenty") {
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 20 &
                        visitor_season_game_num > 20 &
                        (home_season_game_num <= 40 | visitor_season_game_num <= 40)) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      dplyr::matches("backtoback"),
                      dplyr::matches("roll"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "forty") {
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 40 &
                        visitor_season_game_num > 40 &
                        home_season_game_num <= 82 &
                        visitor_season_game_num <= 82) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      dplyr::matches("backtoback"),
                      dplyr::matches("roll"),
                      !!outcomes) %>%
        na.omit()
    }
    if (model == "playoffs") {
      filtered <- raw %>%
        dplyr::filter(home_season_game_num > 82 &
                        visitor_season_game_num > 82) %>%
        dplyr::select(game_id,
                      dplyr::matches("prev_season"),
                      dplyr::matches("prev_game"),
                      dplyr::matches("backtoback"),
                      dplyr::matches("roll"),
                      !!outcomes) %>%
        na.omit()
    }
    # split into inputs and outcomes for preprocessing algorithms
    message("splitting...")
    identifiers <- filtered$game_id
    inputs <- filtered %>%
      dplyr::select(-!!outcomes) %>%
      dplyr::select(-game_id)
    targets <- filtered %>%
      dplyr::select(!!outcomes)
    
    # split the inputs into numeric and non-numeric for recoding
    ## numeric
    message("processing numeric...")
    # TODO: ADD ARGS TO CONFIG FILES
    input_nums <- inputs %>%
      dplyr::select_if(is.numeric)
    num_preproc_obj <- caret::preProcess(input_nums,
                                         method = c("zv", "corr", "center", "scale"),
                                         cutoff = .9)
    num_preproc_data <- predict(num_preproc_obj, input_nums)
    
    ## non-numeric
    message("processing non-numeric...")
    input_non_nums <- inputs %>%
      dplyr::select_if(purrr::negate(is.numeric)) %>%
      dplyr::mutate_all(as.factor)
    dummyvars_obj <- dummyVars(~ ., data = input_non_nums, fullRank = TRUE)
    dummyvars_data <- predict(dummyvars_obj, input_non_nums) %>%
      as.data.frame()
    # tidy names by replacing periods with underscores
    names(dummyvars_data) <- gsub("\\.", "_", names(dummyvars_data))
    # stitch together data elements
    output_data <- dplyr::bind_cols(game_id = identifiers, 
                                    num_preproc_data, 
                                    dummyvars_data)
    # alphabetize columns in case we need to re-order later
    output_data <- output_data[, sort(names(output_data))]
    # add to output list
    modeling_objects[[model]] <- list(preproc = num_preproc_obj,
                                      dummy = dummyvars_obj)
    modeling_data[[model]] <- list(inputs = output_data,
                                   targets = targets)
  }
  # unload caret so it doesn't cause other problems downstream
  detach("package:caret", unload = TRUE)
  # return the lists of stuff
  output_list <- list(modeling_objects = modeling_objects,
                      modeling_data = modeling_data)
  # save the objects to a file if specified
  if (!is.null(objects_output_path)) {
    message("storing output in ", objects_output_path, "...")
    rds_name <- paste0(objects_output_path, "/modeling_objects_",
                       gsub("\\.", "_", as.numeric(Sys.time())),
                       ".rds")
    saveRDS(modeling_objects, 
            file = rds_name)
  }
  return(output_list)
}
