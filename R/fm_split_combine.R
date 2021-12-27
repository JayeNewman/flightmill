#' @name fm_split_combine
#' @title Flight parameters in each mill and combine as two datasets
#' @description Calculate all the flight parameters separately and then combine them as two complete datasets, a summary and a time series dataset. This combines the functions of
#' fm_bouts, fm_dr, fm_speed, fm_summary
#' @param fm_name the flightmill name of the mill
#' @param minimum_bout The allowable time considered to be a bout for the species
#' @param fm_duration Length of time that the flight mill ran for
#' @return A data frame with the flight parameters for the specified flight mill and another data frame with the time series data
#' @export


## Creating a function for calculating all of the flight parameters ----

fm_split_combine <- function(fm_name, minimum_bout, fm_duration)
{
  fm_bouts <- fm_bouts(fm_name, minimum_bout)
  fm_dr <- fm_duration_rest(fm_bouts)
  fm_mean_speed <- fm_speed(fm_bouts)
  fm_summary <- fm_summary(fm_bouts, fm_name, fm_dr, fm_mean_speed, fm_duration)
  return(fm_summary)
}
