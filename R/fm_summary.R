#' @name fm_summary
#' @title Summary of the flight mill data
#' @description Summarises the chamber data
#' @param df Data frame to be modified. Use the data frame created from bouts and rest_dration functions.
#' @param bout_summary_data Data frame of the bouts function.
#' @param bout Vector created from the bouts function.
#' @param mean_speed_bout Vector created from the fm_speed data frame
#' @return A data frame with the bout flight parameters for the specified flight mill chamber
#' @export

fm_summary <- function(bout_data, ch_data, ch_dr, ch_mean_speed, fm_total_duration) {
  bout_data <- bout_data %>%
    dplyr::mutate(max_speed_cms = max(ch_data$speed_cms),
           total_mean_speed_cms = ch_mean_speed$mean_cms_true_bouts,
           distance_km = max(ch_data$distance_cm/100000),
           num_bouts = sum(bout_data$bout == "TRUE"),
           flight_duration = ch_dr$total_flight_duration_sec,
           rest_duration = fm_total_duration - flight_duration) %>%
    dplyr::group_by(bout) %>%
    base::subset(!bout %in% unique(bout[bout == FALSE])) %>%
    dplyr::mutate(run = first(ch_data$run),
           chamber = first(ch_data$chamber),
           id = first(ch_data$id),
           species = first(ch_data$species))
  return(bout_data)
}
