#' @name fm_mean_speed
#' @title Mean speed of the flight mill run
#' @description Calculates the mean bout speed for each bout
#' @param df Data frame to be modified
#' @param bout Uses the vector bout that was created in the flight bouts function
#' @return A data frame with flight parameters calculated for each flight bout.
#' @export

fm_speed <- function(df, bout) {
  df <- df %>%
    dplyr::group_by(bout) %>%
    dplyr::summarise(ttl_mean_bout_speedcms = mean(mean_speed_bout_cms)) %>%
    tidyr::spread(bout, ttl_mean_bout_speedcms) %>%
    dplyr::rename(mean_cms_true_bouts = "TRUE")
}
