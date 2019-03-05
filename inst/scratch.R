rm(list = ls())
devtools::load_all("~/Documents/nbastatstools")
season_boxscores <- lapply(2000:2018,
                           function(x) {
                             message("getting boxscores for ", x)
                             nbastatstools::get_nba_season_boxscores(season = x)
                           }
                          )
season_boxscores_df <- dplyr::bind_rows(season_boxscores)
player_profiles <- 
  nbastatstools::get_nba_player_profiles(playerids = unique(season_boxscores_df$PLAYER_ID))
