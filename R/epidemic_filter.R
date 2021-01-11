epidemic_filter <- function(simulation_data) {

  epidemic <- epidemic_anywhere(simulation_data) %>%
    dplyr::filter(any_epi > 0)


}
