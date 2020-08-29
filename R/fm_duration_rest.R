## Calculating the duration of flight and rest
#' @name fm_duration_rest
#' @title Calculate the duration and rest for bouts
#' @description Calculates the rest and duration for each bout in a flight mill chamber
#' @param df Data frame to be modified. Use the data frame created from bouts function
#' @param bout The bout vector created from the bouts function.
#' @return A data frame with the bout flight parameters for the specified flight mill chamber
#' @export

fm_duration_rest <- function(df, bout) {
  df <- df %>%
    dplyr::group_by(bout) %>%
    dplyr::summarise(total_flight_duration = sum(duration)) %>%
    tidyr::spread(bout, total_flight_duration) %>%
    dplyr::rename(total_flight_duration_sec = "TRUE")
}
