# Calculates speedCMS, speed_kmph, distance_cm, distance_m, and flight bouts
#' @name fm_params
#' @title Calculate flight mill parameters
#' @description Loads multiple files from a single directory. The files are expected to have a first cell that has the flight mill run information. This information is extracted from a selected file and new columns are created with the time information which is parsed to a POSIX format.The files are combined and tidied and extra information on species and chamber for the eight chambers id is required.
#' @param df Data frame to be modified.
#' @param chamber Parameter to be grouped for analysis.
#' @param elapsed_sec Vector of the elapsed seconds parameter.
#' @param counter Vector of the count parameter.
#' @param lowest_speed Lowest acceptable speed for a bout to become FALSE.
#' @param min_time_interval Shortest time interval allowable between infrared breaks. This is to omit unrealistic speeds. The usual acceptable level is 0.01
#' @param fm_duration The length of time in seconds of the flight mill run
#' @return A data frame with flight parameters calculated from elapsed seconds and count grouped by the chambers of the flight mill.
#' @export

fm_params <- function(df, chamber, elapsed_sec, counter, lowest_speed, min_time_interval, fm_duration) {
  df <- df %>%
    dplyr::group_by(chamber) %>%
    dplyr::mutate(time_interval = elapsed_sec - lag(elapsed_sec, default = elapsed_sec[1]),
                  counter_ticker = counter - lag(counter, default = counter[1]),
                  radius_at_break = ifelse(counter_ticker == "0.1" | counter_ticker >= "0.0", 9, -9),
                  speedCMS = radius_at_break / time_interval,
                  speed_kmph = speedCMS * 0.036,
                  distance_cm = cumsum(radius_at_break),
                  distance_m = distance_cm * 0.01,
                  bout = case_when(speedCMS < lowest_speed ~FALSE, speedCMS >= lowest_speed ~TRUE, TRUE ~NA)) %>%
    dplyr::filter(!duplicated(elapsed_sec),
                  time_interval > min_time_interval,
                  elapsed_sec < fm_duration) %>%
    dplyr::select(-counter_ticker);
  return(df)
}