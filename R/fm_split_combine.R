#' @name fm_split_combine
#' @title Flight parameters in each mill and combine as two datasets
#' @description Calculate all the flight parameters separately and then combine them as two complete datasets, a summary and a time series dataset. This combines the functions of
#' ch_bouts, fm_dr, fm_speed, fm_summary
#' @param df Data frame to be used for calculating the flight parameters.
#' @param ch_name
#' @param minimum_bout The allowable time considered to be a bout for the species
#' @param fm_duration Length of time that the flight mill ran for
#' @return A data frame with the flight parameters for the specified flight mill and another data frame with the time series data
#' @export

## Step 3: Chamber split ----
# ungrouping the chambers from the main data frame to get the individual chamber data

fm_split_combine <- function(df, ch_name, minimum_bout, fm_duration) {}



## Creating a function for calculating all of the flight parameters ----

fm_ch_summary <- function(df,  minimum_bout, fm_duration)
{
  ungroup_chambers <- split(df, df$chamber)
  Y <- lapply(seq_along(ungroup_chambers), function(x) as.data.frame(ungroup_chambers[[x]])[, ])

  ch_bouts <- flightmill::fm_bouts(ch_name, minimum_bout)
  ch_dr <- flightmill::fm_duration_rest(ch_bouts)
  ch_mean_speed <- flightmill::fm_speed(ch_bouts)
  ch_summary <- flightmill::fm_summary(ch_bouts, ch_name, ch_dr, ch_mean_speed, fm_duration)

  return(ch_summary)
}


for (i in 1:length(Y))
{
  Y[[i]] <- filterSpikes(Y[[i]])
  ch_summary <- fm_ch_summary(Y[[i]], minimum_bout, fm_duration)

  if (i == 1)
  {
    fm_data_summary <- ch_summary
    long_chamber_data <- Y[[i]]
  }
  else {
    newBit <- Y[[i]]
    long_chamber_data <- rbind(long_chamber_data, newBit)
    fm_data_summary <- rbind(fm_data_summary, ch_summary)
  }
}

length(which(newBit$spikes == 1))
str(long_chamber_data)  ## has a data.frame within a dataframe

long_chamber_data <- long_chamber_data %>%
  filter(spikes == 0)
