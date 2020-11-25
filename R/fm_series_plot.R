#' @name fm_ch_plot
#' @title Time series plots
#' @description Plots for the time series data for each chamber in the flight mill
#' @note TO DO modify this to loop through several of the flight mills
#' @param df data frame input
#' @param title Name of the title of the plot
#' @return A plot of the time series data for a single flight mill
#' @export

fm_ch_plot <- function(df, title) {
  p <- ggplot2::ggplot(df, aes(x=elapsed_sec, y=speed_cms)) +
    geom_line(color = "grey40") +
    #   geom_smooth(method ,color = "orange") +
    xlab("seconds") +
    ylim(0,200) +
    scale_x_continuous(limits = c(0, 3605), breaks = seq(min(0), max(3605), by = 600)) +
    theme(axis.title = element_text(angle = 0, color = "black", size = 12)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(title) +
    theme_classic()
  print(p)
}
