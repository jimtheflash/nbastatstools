% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nba_season_boxscores.R
\name{get_nba_season_boxscores}
\alias{get_nba_season_boxscores}
\title{get nba season boxscores}
\usage{
get_nba_season_boxscores(season = NULL,
  season_types = c("Regular_Season", "Playoffs"))
}
\arguments{
\item{season}{numeric season to get}

\item{season_types}{character vector, which part of the season to get; options include "Regular_Season" and "Playoffs"}
}
\value{
data.frame of player-game boxscores
}
\description{
get nba season boxscores
}
