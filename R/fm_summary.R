#' @name fm_summary
#' @title Summary of the flight mill data
#' @description Summarises the chamber data
#' @param df Data frame to be modified. Use the data frame created from bouts and rest_dration functions.
#' @param bout_summary_data Data frame of the bouts function.
#' @param bout Vector created from the bouts function.
#' @param mean_speed_bout Vector created from the bout data frame
#' @return A data frame with the bout flight parameters for the specified flight mill chamber
#' @export

fm_summary <- function(df, bout_summary_data, bout, mean_speed_bout) {
  df <- df %>%
    dplyr::mutate(max_speed_cms = max(speedCMS),
                  mean_speed_cms = mean(speedCMS),
                  median_speed_cms = median(speedCMS),
                  distance_km = max(distance_cm/100000),
                  num_bouts = sum(bout_summary_data$bout == "TRUE")) %>%
    dplyr::mutate(ttl_mean_bout_speedcms = mean(bout_summary_data$mean_speed_bout_cms))
}
