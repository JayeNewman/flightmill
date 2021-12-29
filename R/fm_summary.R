#' @name fm_summary
#' @title Summary of the flight mill data
#' @description Summarises the mill data
#' @param bout_data Data frame to be modified. Use the data frame created from bouts and rest_duration functions.
#' @param fm_data Flight mill data to be incorporated.
#' @param fm_dr Duration and rest data to be included in the summary.
#' @param fm_mean_speed Vector created from the fm_speed data frame.
#' @param fm_total_duration The duration data to be included in the summary.
#' @return A data frame with the bout flight parameters for the specified flight mill
#' @export

fm_summary <- function(bout_data, fm_data, fm_dr, fm_mean_speed, fm_total_duration) {
  bout_data <- bout_data %>%
    dplyr::mutate(max_speed_cms = max(fm_data$speed_cms),
           total_mean_speed_cms = fm_mean_speed$mean_cms_true_bouts,
           total_median_speed_cms = fm_mean_speed$median_cms_true_bouts,
           distance_km = max(fm_data$distance_cm/100000),
           num_bouts = sum(bout_data$bout == "TRUE"),
           flight_duration = fm_dr$total_flight_duration_sec,
           rest_duration = fm_total_duration - flight_duration) %>%
    dplyr::group_by(bout) %>%
    base::subset(!bout %in% unique(bout[bout == FALSE])) %>%
    dplyr::mutate(run = first(fm_data$run),
                  starting_time = first(fm_data$starting_time),
                  flightmill = first(fm_data$flightmill),
                  #id = first(fm_data$id),
                  species = first(fm_data$species))
  return(bout_data)
}

