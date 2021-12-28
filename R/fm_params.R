# Calculates speed_cms, speed_kmph, distance_cm, distance_m, and flight bouts
#' @name fm_params
#' @title Calculate flight flight mill parameters
#' @description Loads multiple files from a single directory. The files are expected to have a first cell that has the flight flight mill run information. This information is extracted from a selected file and new columns are created with the time information which is parsed to a POSIX format.The files are combined and tidied and extra information on species and for each of the eight flight mills id is required.
#' @param df Data frame to be modified.
#' @param flightmill Parameter to be grouped for analysis.
#' @param elapsed_sec Vector of the elapsed seconds parameter.
#' @param counter Vector of the count parameter.
#' @param sector_distance The part of the circumference that the insect has traveled between each sensor break.
#' @param lowest_speed Lowest acceptable speed for a bout to become FALSE.
#' @param min_time_interval Shortest time interval allowable between infrared breaks. This is to omit unrealistic speeds. The usual acceptable level is 0.01
#' @param fm_duration The length of time in seconds of the flight mill run.
#' @return A data frame is returned with flight parameters calculated from elapsed seconds and count grouped by each flight mill.
#' @export

fm_params <- function(df, flightmill, elapsed_sec, counter, sector_distance, lowest_speed, min_time_interval, fm_duration)
  {
  df <- df %>%
    dplyr::group_by(flightmill) %>%
    dplyr::mutate(counter_ticker = counter - lag(counter, default = counter[1]),
                  radius_at_break = ifelse(counter_ticker == "0.1" | counter_ticker >= "0.0", sector_distance, -sector_distance),
                  counter_ticks = replace(counter_ticker, which(counter_ticker < 0), NA)) %>%
    na.omit() %>%
    dplyr::mutate(time_interval = elapsed_sec - lag(elapsed_sec, default = elapsed_sec[1]),
                  speed_cms = radius_at_break / time_interval,
                  speed_cms = replace(speed_cms, which(speed_cms < 0), 0),
                  speed_kmph = speed_cms * 0.036,
                  speed_kmph = replace(speed_kmph, which(speed_kmph < 0), 0),
                  distance_cm = cumsum(radius_at_break),
                  distance_m = distance_cm * 0.01,
                  bout = case_when(speed_cms < lowest_speed ~FALSE, speed_cms >= lowest_speed ~TRUE, TRUE ~NA)) %>%
    dplyr::filter(!duplicated(elapsed_sec),
                  time_interval > min_time_interval,
                  elapsed_sec < fm_duration)
  df <- df %>%
    group_by(flightmill) %>%
    slice(3:n())

  return(df)
}


# flight5 <- run_name %>%
#   dplyr::group_by(flightmill) %>%
#   dplyr::mutate(counter_ticker = counter - lag(counter, default = counter[1]),
#                 radius_at_break = ifelse(counter_ticker == "0.1" | counter_ticker >= "0.0", 9, -9),
#                 counter_ticks = replace(counter_ticker, which(counter_ticker<0), NA)) %>%
#   na.omit() %>%
#   mutate(time_interval = elapsed_sec - lag(elapsed_sec, default = elapsed_sec[1]),
#          speedCMS = radius_at_break / time_interval,
#          speed_kmph = speedCMS * 0.036,
#          speedCMS = replace(speedCMS, which(speedCMS<0), 0),
#          speed_kmph = replace(speed_kmph, which(speed_kmph<0), 0),
#          distance_cm = cumsum(radius_at_break),
#          distance_m = distance_cm * 0.01,
#          bout = case_when(speedCMS < 20 ~FALSE, speedCMS >= 20 ~TRUE, TRUE ~NA)) %>%
#   dplyr::filter(!duplicated(elapsed_sec),
#                 time_interval > 0.05,
#                 elapsed_sec < 3600)
#
# flight <- flight5 %>%
#   group_by(flightmill) %>%
#   slice(3:n())
