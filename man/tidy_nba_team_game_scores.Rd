% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_nba_team_game_scores.R
\name{tidy_nba_team_game_scores}
\alias{tidy_nba_team_game_scores}
\title{tidy NBA team-game scores}
\usage{
tidy_nba_team_game_scores(scored_team_games = NULL, game_slate = NULL,
  boxscores_for_scoring = NULL)
}
\arguments{
\item{scored_team_games}{list of model outputs from \code{nbastatstools::score_nba_team_games()}}

\item{game_slate}{game slate output from \code{nbastatstools::tidy_game_slate()}}

\item{boxscores_for_scoring}{boxscore data to extract team abbreviations}
}
\value{
tidy data.frame of nba team-game scores
}
\description{
tidy NBA team-game scores
}
