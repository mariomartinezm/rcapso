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

#' Computes the mean field approximation of the CaPso model given a set of
#' rules.
#'
#' @param rules A character vector containing the transition functions to use
#' in the computation of the mean field model. Defaults to
#' c("ic", "rep_preds", "death_preds", "death_preys", "rep_preys")
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
rcapso_compute_mf <- function(rules = c("ic", "rep_preds", "death_preds",
                                        "death_preys", "rep_preys"),
                              use_reg = TRUE, num_iter = 100,
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
      } else if (f == "death_preds_adj") {
        phi_t <- rcapso_mf_death_of_predators_adj(psi_t, phi_t, phi[t])
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

#' Computes the mean field term for the intraspecific competition stage.
#'
#' @param density The initial density of the population of preys.
#' @param alpha The intraspecific competition coefficient.
#'
#' @return The new size of the population after the intraspecific competition
#' stage.
#'
#' @noRd
rcapso_mf_ic <- function(density, alpha) {
  density - alpha * density ^ 2
}

#' Computes the mean field term for the reproduction stage.
#'
#' @param density The initial density of the population.
#' @param epsilon The reproductive capacity of the population.
#' @param radius The radius of the reproduction neighborhood.
#'
#' @return The new size of the population after the reproduction stage.
#'
#' @noRd
rcapso_mf_reproduction <- function(density, epsilon, radius) {
  # Calculate the probability of an 'event' occurring in the neighboord of an
  # individual
  card <- (2 * radius + 1) ^ 2 - 1
  p    <- 1 / card

  num_preds         <- card * density
  max_num_of_births <- epsilon * num_preds

  density + (1 - density) * (1 - (1 - p) ^ max_num_of_births)
}

#' Computes the mean field term for the death of predators.
#'
#' @param psi The initial density of the population of preys.
#' @param phi The initial density of the population of predators.
#' @param a The coefficient of the line that defines predator mortality.
#' @param b The intercept of the line that defines predator mortality.
#'
#' @return The new size of the population of predators after the death of
#' predators stage.
#'
#' @noRd
rcapso_mf_death_of_predators <- function(psi, phi, a, b) {
  phi - (b + a * psi) * phi
}

#' Computes the mean field term for the death of predators (adjusted).
#'
#' @param psi The initial density of the population of preys.
#' @param phi The initial density of the population of predators after
#' reproduction.
#' @param phi_prev The density of the population of predators before
#' reproduction.
#'
#' @return The new size of the population of predators after the death of
#' predators stage.
#'
#' @noRd
rcapso_mf_death_of_predators_adj <- function(psi, phi_r, phi_t) {
    phi_r - phi_t - (1 - psi) * (phi_r - phi_t)
}

#' Computes the mean field term for the death of preys.
#'
#' @param psi The density of the population of preys.
#' @param phi The density of the population of predators.
#' @param use_reg Indicates if regression terms should be used.
#' @param d The coefficient of the line that defines prey mortality.
#' @param e The intercept of the line that defines prey mortality.
#'
#' @return The new size of the population of preys after the death of preys
#' stage.
#'
#' @noRd
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

#' Adjusts the parameters of the mean field model using simulation data
#'
#' @param df A data frame containing normalize data of a CaPso simulation.
#' @param transient_length The length (in seasons) of the transient.
#'
#' @return A numerical vector containing the adjusted parameters of the mean
#' field model.
#'
#' @export
rcapso_adjust_parameters <- function(df, transient_length = 1000) {
  # Remove transient
  data <- df %>% dplyr::slice_tail(n = 9000)

  # Estimate a and b
  fit <- stats::lm(preds_dp ~ preys_dbpd, data = data)
  a <- summary(fit)$coefficients["preys_dbpd", "Estimate"]
  b <- summary(fit)$coefficients["(Intercept)", "Estimate"]

  # Estimate d and e
  fit <- stats::lm(preys_dp ~ preds_dbpd,
          data = data)
  d <- summary(fit)$coefficients["preds_dbpd", "Estimate"]
  e <- summary(fit)$coefficients["(Intercept)", "Estimate"]

  # Estimate epsilon_Y
  radius <- 1
  card   <- (2 * radius + 1) ^ 2 - 1
  p      <- 1 / card
  f      <- PreyBirthRate ~ (1 - preys_dbr) * (1 - (1 - p) ^
                                              (epsilon * card * preys_dbr))
  fit <- stats::nls(f, data = data, start = list(epsilon = 1))
  ey  <- stats::coef(fit)[1]

  # Estimate epsilon_Z
  f   <- PredatorBirthRate ~ (1 - preds_dbr) * (1 - (1 - p) ^
                                               (epsilon * card * preds_dbr))
  fit <- stats::nls(f, data = data, start = list(epsilon = 2))
  ez  <- stats::coef(fit)[1]

  stats::setNames(c(a, b, d, e, ey, ez), c("a", "b", "d", "e", "ey", "ez"))
}
