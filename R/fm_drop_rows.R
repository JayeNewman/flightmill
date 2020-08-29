# Removing the first number of lines from the data frame
#' @name fm_drop_nrows
#' @title Drop the first n number of rows
#' @description Drop the first set of rows from data frame. drops the fist n number of rows of the data as it is the first few rows that will be incorrect values as the time of the first two values will be part way through the hole in the disc.
#' @param df the dataframe required
#' @param N number of rows that need to be removed from the start of the file
#' @return Output to a data frame with the first n number of rows removed
#' @export

fm_drop_nrows <- function(df,N) {
  df[-(1:N), , drop = FALSE]
}
