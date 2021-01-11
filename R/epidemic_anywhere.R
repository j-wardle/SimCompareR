epidemic_anywhere <- function(simulation_data) {

  # global_epidemics <- simulation_data %>%
  #   dplyr::filter(time == max(time) & variable == "recovered") %>%
  #   dplyr::mutate(epi_flag = mmand::threshold(value, method = "kmeans")) %>%
  #   dplyr::group_by(sim) %>%
  #   dplyr::mutate(number_epi = sum(epi_flag)) %>%
  #   dplyr::mutate(any_epi_flag = ifelse(number_epi > 0, 1, 0))

  global_epidemics <- simulation_data %>%
    dplyr::filter(time == max(time) & variable == "recovered") %>%
    dplyr::mutate(epi_flag = mmand::threshold(value, method = "kmeans")) %>%
    dplyr::group_by(sim) %>%
    dplyr::mutate(number_epi = sum(epi_flag)) %>%
    dplyr::filter(number_epi > 0)

  epi_sims <- global_epidemics$sim

  simulation_data %>%
    dplyr::mutate(any_epi = ifelse(sim %in% epi_sims, 1, 0))

}

epidemic_filter <- function(simulation_data) {

  epidemic <- epidemic_anywhere(simulation_data) %>%
    dplyr::filter(any_epi > 0)

}


