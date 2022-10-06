#' Gets the rcapso theme for ggplot
#'
#' @return The ggplot2 theme object for the rcapso package.
rcapso_get_theme <- function() {
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
  return(theme)
}

#' Plots the time series of preys and predators.
#'
#' @param data A data.frame containing the time series of preys
#' and predators.
#' @param cols The columns in the dataframe that contain the
#' time series of preys and predators.
#' @param title The title for the plot.
#' @param xlabel The label for the x axis.
#' @param ylabel The label for the y axis.
#' @param normalize TRUE if the data needs to be converted to a density.
#' @param lat_size The size of the lattice (only used if normalize is TRUE).
#'
#' @export
rcapso_plot_prey_pred_data <- function(data, cols = c("Preys", "Predators"),
                                       title = "Prey-Predator time series",
                                       xlabel = "Time (Seasons)",
                                       ylabel = "Population density",
                                       normalize = TRUE, lat_size = 262144) {
  colors <- RColorBrewer::brewer.pal(12, "Paired")

  opar   <- graphics::par(no.readonly = TRUE)
  graphics::par()

  index_set <- seq_len(nrow(data))

  if (normalize == TRUE) {
      data[, cols[1]] <- data[, cols[1]] / lat_size
      data[, cols[2]] <- data[, cols[2]] / lat_size
  }

  graphics::plot(index_set, data[, cols[1]], ylim = c(0, 1),
                 type = "l", col = colors[4], lwd = 2,
                 xlab = xlabel, ylab = ylabel)
  graphics::lines(index_set, data[, cols[2]],
                  type = "l", col = colors[6], lwd = 2)

  graphics::title(main = title)

  graphics::legend("topright", title = "Species:", cols, inset = 0.03,
                   lty=c(1, 1), col = colors[c(4, 6)])

  graphics::par(opar)
}

#' Plots the phase plot of two time series contained in parameter data
#'
#' @param data A two-column matrix containing two time series.
#' @param title The title for the plot.
#' @param xlabel The label for the x axis.
#' @param ylabel The label for the y axis.
#'
#' @export
rcapso_plot_phase <- function(data, title = "", xlabel = "", ylabel = "") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = data[, 1], y = data[, 2])) +
    ggplot2::geom_path(color = "black") +
    ggplot2::geom_point(shape = 21,
                        color = "black",
                        fill = "#69b3a2",
                        size = 4) +
    ggplot2::labs(x = xlabel, y = ylabel, title = title) +
    rcapso_get_theme()
}

#' Plots the Fourier spectrum of a time series.
#'
#' @param x A numeric vector containing the data of the time series.
#' @param method_spec String specifying the method used to estimate the
#' spectral density.
#'
#' @return A list containing the cycles sorted in order of magnitude of the
#' power spikes.
#'
#' @export
rcapso_plot_fourier_spectrum <- function(x, method_spec) {
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
