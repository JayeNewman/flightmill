#' @name fm_filter_spikes
#' @title Identify anomolies in the flight mill dataset
#' @description Identifies anomolies in the flight mill dataset by using a madThreshold and creates a column that is either a 1: it is a spike and 0: is not a spike
#' @param x the data frame from which to identify the anomolies
#' @param madThreshold
#' @return A data frame with summary data and another dataframe with the complete time series data
#' @export
#'

fm_filter_spikes <- function(x, madThreshold = 4)
{
  diffX <- c(0, diff(x$speed_cms))
  diffX <- diffX/mad(diffX)
  spikes <- rep(0, length(diffX))
  for(i in 1:(length(diffX) - 1))
  {
    if(diffX[i] > madThreshold & -1*diffX[i + 1] > madThreshold)
    {
      spikes[i] <- 1
    }
    if(-1*diffX[i] > madThreshold & diffX[i + 1] > madThreshold)
    {
      spikes[i] <- 1
    }
  }
  x <- cbind(x, spikes)
  return(x)
}
