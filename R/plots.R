theme <- ggplot2::theme(plot.title =
                          ggplot2::element_text(face = "bold.italic",
                                                size = 14,
                                                color = "black"),
                        axis.title =
                          ggplot2::element_text(face = "bold",
                                                size = 12,
                                                color = "black"),
                        axis.text =
                          ggplot2::element_text(face = "bold",
                                                size = 9,
                                                color = "black"),
                        panel.background =
                          ggplot2::element_rect(fill = "white",
                                                color = "white"),
                        panel.grid.major.x =
                          ggplot2::element_line(color = "lightgray"),
                        panel.grid.major.y =
                          ggplot2::element_line(color = "lightgray"),
                        panel.grid.minor.x =
                          ggplot2::element_line(color = "lightgray"),
                        panel.grid.minor.y =
                          ggplot2::element_line(color = "lightgray"))

#' Plots the time series of preys and predators.
#' @param data A two-column matrix containing the data of preys and predators.
#' @param title The title for the plot.
#' @param xlabel The label for the x axis.
#' @param ylabel The label for the y axis.
#' @export
capso_plot_prey_pred_data <- function(data, title = "",
                                      xlabel = "", ylabel = "") {
  index_set <- 1:nrow(data)

  ggplot2::ggplot() +
    ggplot2::geom_line(data = data,
                       ggplot2::aes(index_set, y = data[, 1]),
                       color = "#69b3a2", size = 1) +
    ggplot2::geom_line(data = data,
                       ggplot2::aes(index_set, y = data[, 2]),
                       color = "red", size = 1) +
    ggplot2::labs(x = xlabel, y = ylabel, title = title) +
    theme
}

#' Plots the phase plot of two time series contained in parameter data
#' @param data A two-column matrix containing two time series.
#' @param title The title for the plot.
#' @param xlabel The label for the x axis.
#' @param ylabel The label for the y axis.
#' @export
capso_plot_phase <- function(data, title = "", xlabel = "", ylabel = "") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = data[, 1], y = data[, 2])) +
    ggplot2::geom_path(color = "black") +
    ggplot2::geom_point(shape = 21,
                        color = "black",
                        fill = "#69b3a2",
                        size = 4) +
    ggplot2::labs(x = xlabel, y = ylabel, title = title) +
    theme
}

#' Plots the Fourier spectrum of a time series.
#' @param x A numeric vector containing the data of the time series.
#' @param method_spec String specifying the method used to estimate the
#' spectral density.
#' @return A list containing the cycles sorted in order of magnitude of the
#' power spikes.
#' @export
capso_plot_fourier_spectrum <- function(x, method_spec) {
  if (method_spec) {
    spec_out <- stats::spectrum(x, method = "ar")
  } else {
    spec_out <- stats::spectrum(x, method = "pgram")
  }

  power     <- spec_out$spec      # Spectral values (vertical axis of plots)
  frequency <- spec_out$freq      # Frequencies on horizontal axis of plot
  cycle     <- 1 / frequency

  # Sort cycles in order of magnitude of power spikes
  hold <- matrix(0, (length(power) - 2), 1)
  for (i in 1:(length(power) - 2)) {
    max1      <- if (power[i + 1] > power[i] &&
                     power[i + 1] > power[i + 2]) 1 else (0)
    hold[i, ] <- max1
  }

  max         <- which(hold == 1) + 1
  power_max   <- power[max]
  cycle_max   <- cycle[max]
  o           <- order(power_max, decreasing = TRUE)
  cycle_max_o <- cycle_max[o]
  results     <- list(cycle_max_o)

  return(results)
}
