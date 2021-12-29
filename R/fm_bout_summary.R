# Bout summary which includes mean speed and duration into the data frame for the mill

#' @name fm_bout_summary
#' @title Calculate mean true speed and flight duration for each bout
#' @description Summarises the mean true flight speed and the flight duration for each bout for the individual flightmill
#' @param df Data frame to be modified. Use the data frame created from bouts function
#' @param fm_data The data frame for the individual flightmill
#' @param bout the vector bout created from the fm_bouts function
#' @param fm_total_duration The total duration that the flight mill ran for. This is used to calculate the rest duration by taking away from the flight duration that is calculated
#' @return A data frame with the bout flight parameters for the specified flight mill
#' @export

fm_bout_summary <- function(df, fm_data, bout, fm_total_duration){
  # summarising the mean total speed
    mean_speed <- df %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::group_by(bout) %>%
      dplyr::summarise(ttl_mean_bout_speed_cms = mean(mean_speed_bout_cms),
                       ttl_median_bout_speed_cms = median_speed_bout_cms) %>%
      tidyr::spread(bout, ttl_mean_bout_speed_cms) %>%
      tidyr::spread(bout, ttl_median_bout_speed_cms) %>%
      dplyr::rename(mean_cms_true_bouts = "TRUE",
                    median_cms_true_bouts = "TRUE")


  # calculating duration of actual flight
    flight_duration <- df %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::group_by(bout) %>%
      dplyr::summarise(total_flight_duration = sum(duration)) %>%
      tidyr::spread(bout, total_flight_duration) %>%
      dplyr::rename(total_flight_duration_sec = "TRUE")

    # combining the data to make a proper bout summary data frame
    bout_summary <- df %>%
      dplyr::mutate(max_speed_cms = max(fm_data$speed_cms),
                    total_mean_speed_cms = mean_speed$mean_cms_true_bouts,
                    total_median_speed_cms = mean_speed$median_cms_true_bouts,
                    distance_km = max(fm_data$distance_cm/100000),
                    num_bouts = sum(df$bout == "TRUE"),
                    flight_duration_sec = flight_duration$total_flight_duration_sec)
      dplyr::group_by(bout) %>%
      base::subset(!bout %in% unique(bout[bout == FALSE])) %>%
      dplyr::mutate(run = first(fm_data$run),
                    flightmill = first(fm_data$flightmill),
                    #id = first(fm_data$id),
                    species = first(fm_data$species))
    return(bout_summary)
}
