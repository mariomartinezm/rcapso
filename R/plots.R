theme <- ggplot2::theme(plot.title = ggplot2::element_text(face = "bold.italic",
                                                           size = 14,
                                                           color = "black"),
                        axis.title = ggplot2::element_text(face = "bold",
                                                           size = 12,
                                                           color = "black"),
                        axis.text = ggplot2::element_text(face = "bold",
                                                          size = 9,
                                                          color = "black"),
                        panel.background = ggplot2::element_rect(fill = "white",
                                                                 color = "white"),
                        panel.grid.major.x = ggplot2::element_line(color = "lightgray"),
                        panel.grid.major.y = ggplot2::element_line(color = "lightgray"),
                        panel.grid.minor.x = ggplot2::element_line(color = "lightgray"),
                        panel.grid.minor.y = ggplot2::element_line(color = "lightgray"))

#' Plots the phase plot of two time series contained in parameter data
#' @param data A two-column matrix containing two time series.
#' @param title The title for the plot.
#' @param xlabel The label for the x axis.
#' @param ylabel The label for the y axis.
#' @export
capso_phase_plot <- function(data, title="", xlabel="", ylabel="") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = data[, 1], y = data[, 2])) +
    ggplot2::geom_path(color="black") +
    ggplot2::geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
    ggplot2::labs(x = xlabel, y = ylabel, title = title) +
    theme
}
