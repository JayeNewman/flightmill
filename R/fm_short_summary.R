#' @name fm_short_summary
#' @title Short flight mill Summary
#' @description Summarises the flight mill data.
#' @param df Data frame to be modified. Use the data frame created from bouts and rest_duration functions.
#' @param bout_summary_data Data frame of the bouts function.
#' @param duration_rest_data Vector created from the rest_duration function.
#' @return A data frame with the bout flight parameters for the specified flight mill.
#' @importFrom magrittr %>%
#' @export

fm_short_summary <- function(df, bout_summary_data, duration_rest_data) {
  df <- df %>%
    base::unique() %>%
    base::merge(bout_summary_data) %>%
    base::merge(duration_rest_data)
}
