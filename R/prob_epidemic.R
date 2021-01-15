#' Probability of observing an epidemic in simulations
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return The probability that the simulated data contains at least one patch where an epidemic is observed.
#' @export
#'

prob_sim_epidemic <- function(simulation_data) {

  any_epidemic <- epidemic_anywhere(simulation_data) %>%
    dplyr::group_by(sim) %>%
    dplyr::summarise(epidemic = mean(any_epi))

  sum(any_epidemic$epidemic > 0) / nrow(any_epidemic)

}
