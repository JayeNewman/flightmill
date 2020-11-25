## Summarising the number of bouts for the flight run

#' @name fm_bouts
#' @title Summarising number of bouts in a flight run
#' @description The number of bouts is calculated for each flight mill chamber.
#' @note TO DO modify this to loop through several of the flight mill chambers
#' @param df data frame input
#' @param min_acceptable_duration The minimum acceptable duration of a stop to be considered a bout = FALSE.    Duration of flight must be more than 2.5 seconds of sustained flight
#' @return A data frame with flight bout parameters calculated
#' @export

fm_bouts <- function(df, min_acceptable_duration) {
  CHrle <- df$bout %>% rle()
  num_groups <- length(CHrle$values)
  group_column <- purrr::map2(seq(num_groups), CHrle$lengths, ~rep(.x, .y)) %>%
    unlist()

  df <- df %>%
    dplyr::mutate(bout_group = group_column) %>%
    dplyr::group_by(bout_group) %>%
    dplyr::summarise(start = min(elapsed_sec),
                     end = max(elapsed_sec),
                     bout_rest_distance_cm = sum(radius_at_break),
                     bout = first(bout),
                     mean_speed_bout_cms = mean(speed_cms)) %>%
    dplyr::mutate(duration = end - start) %>%
    dplyr::filter(!duration < min_acceptable_duration)
}


