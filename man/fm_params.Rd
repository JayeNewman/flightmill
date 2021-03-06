% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_params.R
\name{fm_params}
\alias{fm_params}
\title{Calculate flight mill parameters}
\usage{
fm_params(
  df,
  chamber,
  elapsed_sec,
  counter,
  lowest_speed,
  min_time_interval,
  fm_duration
)
}
\arguments{
\item{df}{Data frame to be modified.}

\item{chamber}{Parameter to be grouped for analysis.}

\item{elapsed_sec}{Vector of the elapsed seconds parameter.}

\item{counter}{Vector of the count parameter.}

\item{lowest_speed}{Lowest acceptable speed for a bout to become FALSE.}

\item{min_time_interval}{Shortest time interval allowable between infrared breaks. This is to omit unrealistic speeds. The usual acceptable level is 0.01}

\item{fm_duration}{The length of time in seconds of the flight mill run}
}
\value{
A data frame with flight parameters calculated from elapsed seconds and count grouped by the chambers of the flight mill.
}
\description{
Loads multiple files from a single directory. The files are expected to have a first cell that has the flight mill run information. This information is extracted from a selected file and new columns are created with the time information which is parsed to a POSIX format.The files are combined and tidied and extra information on species and chamber for the eight chambers id is required.
}
