prob_epidemic <- function(simulation_data) {

  any_epidemic <- epidemic_anywhere(simulation_data) %>%
    dplyr::group_by(sim) %>%
    dplyr::summarise(epidemic = mean(any_epi))

  sum(any_epidemic$epidemic > 0) / nrow(any_epidemic)

}
