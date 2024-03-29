#' @name fm_filter_spikes
#' @title Identify anomalies in the flight mill data set
#' @description Identifies anomalies in the flight mill data set by using the median absolute deviation of the flight speed and creates a column that is either a 1: it is a spike and 0: is not a spike
#' @param x Data frame
#' @param madThreshold Computes the median absolute deviation using a numeric vector threshold
#' @return A data frame with summary data and another data frame with the complete time series data
#' @export
#'

fm_filter_spikes <- function(x, madThreshold = 4)
{
  diffX <- c(0, diff(x$speed_cms))
  diffX <- diffX/mad(diffX)
  spikes <- rep(0, length(diffX))
  for(i in 1:(length(diffX) - 1))
  {
    if(diffX[i] > madThreshold) # & -1*diffX[i + 1] > madThreshold)  # this calculates when it has a double spike, just before and just after
    {
      spikes[i] <- 1
    }
    if(-1*diffX[i] > madThreshold) # & diffX[i + 1] > madThreshold)
    {
      spikes[i] <- 1
    }
  }
  x <- cbind(x, spikes)
  return(x)
}
