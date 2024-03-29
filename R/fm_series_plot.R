#' @name fm_series_plot
#' @title Time series plots
#' @description Plots for the time series data for each flightmill
#' @note TO DO modify this to loop through several of the flightmills
#' @param df data frame input
#' @param title Name of the title of the plot
#' @return A plot of the time series data for a single flightmill
#' @export

fm_series_plot <- function(df, title) {
  p <- ggplot2::ggplot(df, aes(x=elapsed_sec, y=speed_cms)) +
    ggplot2::geom_line(color = "grey40") +
    #   geom_smooth(method ,color = "orange") +
    ggplot2::xlab("seconds") +
    ggplot2::ylim(0,200) +
    ggplot2::scale_x_continuous(limits = c(0, 3605), breaks = seq(min(0), max(3605), by = 600)) +
    ggplot2::theme(axis.title = element_text(angle = 0, color = "black", size = 12)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_classic()
  print(p)
}
