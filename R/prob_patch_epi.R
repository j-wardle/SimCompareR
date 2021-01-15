#' Probability of observing an epidemic in patch
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return The probability that a patch has an epidemic (given there is one somewhere).
#' @export
#'

prob_patch_epidemic <- function(simulation_data) {

  epidemic_filter(simulation_data) %>%
    dplyr::group_by(patch) %>%
    dplyr::summarise(prob_patch_epidemic = sum(patch_epi) / dplyr::n()) %>%
    dplyr::select(patch, prob_patch_epidemic)

  }
