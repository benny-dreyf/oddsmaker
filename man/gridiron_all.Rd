% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridiron_all.R
\name{gridiron_all}
\alias{gridiron_all}
\title{After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. % share of bet volume for each team by game using this function}
\usage{
gridiron_all(dataset, week_no)
}
\arguments{
\item{dataset}{dataframe or tibble containing public consensus share of bets and game line for multiples games for a given week pulled more than once by odds_consensus function from oddsmaker package}

\item{week_no}{designates the week number of the NFL season that you want to plot}
}
\value{
a set of times-series charts showing percent of public consensus and lines movement for all nfl games during a given week
}
\description{
After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. % share of bet volume for each team by game using this function
}
