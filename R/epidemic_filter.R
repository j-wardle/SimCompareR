#' Filter on whether any epidemic was observed in simulation.
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return A subset of the simulation_data in which an epidemic was observed in at least one patch.
#' @export
#'

epidemic_filter <- function(simulation_data) {

  epidemic <- epidemic_anywhere(simulation_data) %>%
    dplyr::filter(any_epi > 0)

  epidemic

}
