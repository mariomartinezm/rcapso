#' Gets the mean field approximation of the CaPso model.
#'
#' @param use_reg Indicates if regression terms should be used.
#' @param num_iter The number of iterations to simulate.
#' @param psi0 The initial density of the prey population.
#' @param phi0 The initial density of the predator population.
#' @param alpha The intraspecific competition coefficient.
#' @param ey The reproductive capacity of preys.
#' @param ry The radius of the prey's reproduction neighborhood.
#' @param ez The reproductive capacity of predators.
#' @param rz The radius of the predator's reproduction neighborhood.
#' @param a The coefficient of the line that defines predator mortality.
#' @param b The intercept of the line that defines predator mortality.
#' @param d The coefficient of the line that defines prey mortality.
#' @param e The intercept of the line that defines prey mortality.
#'
#' @return A 2-column data frame containing the time series of preys and
#' predators.
#' @export
rcapso_mean_field <- function(use_reg = TRUE, num_iter = 100,
                              psi0 = 1, phi0 = 0.01, alpha = 0.1,
                              ey = 1, ry = 1, ez = 1, rz = 1,
                              a = -1, b = 1, d = 1, e = 0) {
  # Initialize data structures
  index_set <- seq(from = 1, to = num_iter, by = 1)
  psi       <- numeric(length(index_set))
  phi       <- numeric(length(index_set))

  # Calculate the probability of an 'event' occurring in the neighboord of a
  # prey or predator
  card_mry  <- (2 * ry + 1) ^ 2 - 1
  py        <- 1 / card_mry
  card_mrz  <- (2 * rz + 1) ^ 2 - 1
  pz        <- 1 / card_mrz

  # Initialize population densities and simulate the model
  psi[1] <- psi0
  phi[1] <- phi0

  for (t in seq_len(length(index_set) - 1)) {
    # Intraspecific competition
    psi_ic <- psi[t] - alpha * psi[t] ^ 2

    # Reproduction of predators
    num_preds          <- card_mrz * phi[t]
    max_num_of_births  <- ez * num_preds
    phi_r              <- phi[t] + (1 - phi[t]) * (1 - (1 - pz) ^
                                                   max_num_of_births)

    # Death of Predators
    phi[t + 1] <- phi_r - (b + a * psi_ic) * phi_r

    # Death Of Preys
    if (use_reg) {
      psi_d <- psi_ic - (e + d * phi[t + 1]) * psi_ic
    } else {
      psi_d <- psi_ic - phi[t + 1]
    }

    # Reproduction of preys
    num_preys         <- card_mry * psi_d
    max_num_of_births <- ey * num_preys
    psi[t + 1]        <- psi_d + (1 - psi_d) * (1 - (1 - py) ^
                                                max_num_of_births)
  }

  data.frame(Preys = psi, Predators = phi, stringsAsFactors = FALSE)
}

rcapso_compute_mf <- function(rules, use_reg = TRUE, num_iter = 100,
                              psi0 = 1, phi0 = 0.01, alpha = 0.1,
                              ey = 1, ry = 1, ez = 1, rz = 1,
                              a = -1, b = 1, d = 1, e = 0) {

  index_set <- seq(from = 1, to = num_iter, by = 1)

  psi       <- numeric(length(index_set))
  psi[1]    <- psi0
  psi_t     <- psi[1]

  phi       <- numeric(length(index_set))
  phi[1]    <- phi0
  phi_t     <- phi[1]

  for (t in seq_len(length(index_set) - 1)) {
    for (f in rules) {
      if (f == "ic") {
        psi_t <- rcapso_mf_ic(psi_t, alpha)
      } else if (f == "rep_preds") {
        phi_t <- rcapso_mf_reproduction(phi_t, ez, rz)
      } else if (f == "death_preds") {
        phi_t <- rcapso_mf_death_of_predators(psi_t, phi_t, a, b)
      } else if (f == "death_preys") {
        psi_t <- rcapso_mf_death_of_preys(psi_t, phi_t, use_reg, d, e)
      } else if (f == "rep_preys") {
        psi_t <- rcapso_mf_reproduction(psi_t, ey, ry)
      }
    }

    psi[t + 1] <- psi_t
    phi[t + 1] <- phi_t
  }

  data.frame(Preys = psi, Predators = phi, stringsAsFactors = FALSE)
}

rcapso_mf_ic <- function(density, alpha) {
  density - alpha * density ^ 2
}

rcapso_mf_reproduction <- function(density, epsilon, radius) {
  # Calculate the probability of an 'event' occurring in the neighboord of an
  # individual
  card <- (2 * radius + 1) ^ 2 - 1
  p    <- 1 / card

  num_preds         <- card * density
  max_num_of_births <- epsilon * num_preds

  density + (1 - density) * (1 - (1 - p) ^ max_num_of_births)
}

rcapso_mf_death_of_predators <- function(psi, phi, a, b) {
  phi - (b + a * psi) * phi
}

rcapso_mf_death_of_preys <- function(psi, phi, use_reg, d, e) {
    if (use_reg) {
      return(psi - (e + d * phi) * psi)
    } else {
      return(psi - phi)
    }
}

#' Calculates the mean field growth rate of a population
#'
#' @param pop_data A numeric vector containing populatin density data.
#' @param epsilon The reproductive capacity of the population.
#' @param radius The reproductive capacity of the population.
#'
#' @return A numerical vector with the mean growth rate for every value in
#' pop_data.
#'
#' @export
rcapso_mean_field_growth_rate <- function(pop_data, epsilon = 1, radius = 1) {
  card <- (2 * radius + 1) ^ 2 - 1
  p    <- 1 / card

  (1 - pop_data) * (1 - (1 - p) ^ (epsilon * card * pop_data))
}
