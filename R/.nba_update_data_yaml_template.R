#' generate a template yaml
#' @return a yaml to be filled in
.nba_pipeline_yaml_template <- function() {
  yaml::as.yaml(
  list(db = list(dbname = "",
                 user = "",
                 password = ""),
       filenames = list(boxscores = "",
                        team_games = ""),
       paths = list(ingestion_path = "",
                    boxscores_path = "",
                    team_games_path = ""),
       tables = list(boxscores_table = "",
                     team_games_table = ""))
  )
}

