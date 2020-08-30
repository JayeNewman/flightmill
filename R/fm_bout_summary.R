# Bout summary which includes mean speed and duration into the data frame for the chamber

#' @name fm_bout_summary
#' @title Calculate mean true speed and flight duration for each bout
#' @description Summarises the mean true flight speed and the flight duration for each bout for the individual chamber
#' @param df Data frame to be modified. Use the data frame created from bouts function
#' @param ch_data The data frame for the individual chamber within the flight mill
#' @return A data frame with the bout flight parameters for the specified flight mill chamber
#' @export

fm_bout_summary <- function(df, ch_data){
  # summarising the mean total speed
    mean_speed <- df %>%
      dplyr::group_by(bout) %>%
      dplyr::summarise(ttl_mean_bout_speedcms = mean(mean_speed_bout_cms)) %>%
      tidyr::spread(bout, ttl_mean_bout_speedcms) %>%
      dplyr::rename(mean_cms_true_bouts = "TRUE")

  # calculating duration of actual flight
    flight_duration <- df %>%
      dplyr::group_by(bout) %>%
      dplyr::summarise(total_flight_duration = sum(duration)) %>%
      tidyr::spread(bout, total_flight_duration) %>%
      dplyr::rename(total_flight_duration_sec = "TRUE")

    # combining the data to make a proper bout summary data frame
    bout_summary <- df %>%
      dplyr::mutate(max_speed_cms = max(ch_data$speedCMS),
                    total_mean_speed_cms = mean_speed$mean_cms_true_bouts,
                    distance_km = max(ch_data$distance_cm/100000),
                    num_bouts = sum(df$bout == "TRUE"),
                    flight_duration = flight_duration$total_flight_duration_sec,
                    rest_duration = fm_total_duration - flight_duration) %>%
      dplyr::group_by(bout) %>%
      base::subset(!bout %in% unique(bout[bout == FALSE])) %>%
      dplyr::mutate(run = first(ch_data$run),
                    chamber = first(ch_data$chamber),
                    id = first(ch_data$id),
                    species = first(ch_data$species))
    return(bout_summary)
}
