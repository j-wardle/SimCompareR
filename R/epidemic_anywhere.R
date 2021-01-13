#' Identify if an epidemic occurred in any patch over the course of the simulation.
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model.
#'
#' @return simulation_data with additional variable (any_epi) showing whether there were any epidemics.
#'

epidemic_anywhere <- function(simulation_data, method = "bins") {

  compartments <- c("susceptible", "exposed", "infected", "recovered")

  sim_population <- simulation_data %>%
    dplyr::filter(variable %in% compartments) %>%
    dplyr::group_by(sim, patch, time) %>%
    dplyr::mutate(population = sum(value))

  epidemic_size <- sim_population %>%
    dplyr::ungroup() %>%
    dplyr::filter(time == max(time) & variable == "recovered") %>%
    dplyr::mutate(size = 1000 * value / population)

  if (method == "bins") {

    epidemic_size$bin <- cut(epidemic_size$size, 100)

    thresh_bins <- epidemic_size$bin
    thresh_bins <- forcats::fct_count(epidemic_size$bin)

    threshold <- thresh_bins %>%
      dplyr::filter(n == 0 &
                      dplyr::lead(n, 1) == 0 &
                      dplyr::lead(n, 2) == 0)

    threshold <- threshold[[1,1]]

    global_epidemics <- epidemic_size %>%
      dplyr::mutate(patch_epi = ifelse(as.integer(bin) < as.integer(threshold),
                                      0, 1)) %>%
      dplyr::group_by(sim) %>%
      dplyr::mutate(sim_epi_number = sum(patch_epi))

    global_epi_filter <- dplyr::filter(global_epidemics, sim_epi_number > 0)
    epi_sims <- global_epi_filter$sim

    patch_sim_epi <- global_epidemics %>%
      dplyr::select(sim, patch, patch_epi, sim_epi_number)

    sim_epidemics <- dplyr::left_join(simulation_data, patch_sim_epi,
                                      by = c("sim", "patch")) %>%
      dplyr::mutate(any_epi = ifelse(sim %in% epi_sims, 1, 0))

    sim_epidemics

    # simulation_data %>%
    #   dplyr::mutate(patch_epi = global_epidemics$epi_flag,
    #                 sim_epi_number = global_epidemics$number_epi,
    #                 any_epi = ifelse(sim %in% epi_sims, 1, 0))

  } else if (method == "kmeans") {

  global_epidemics <- epidemic_size %>%
    dplyr::mutate(epi_flag = mmand::threshold(size, method = "kmeans")) %>%
    dplyr::group_by(sim) %>%
    dplyr::mutate(number_epi = sum(epi_flag)) %>%
    dplyr::filter(number_epi > 0)

  epi_sims <- global_epidemics$sim

  simulation_data %>%
    dplyr::mutate(any_epi = ifelse(sim %in% epi_sims, 1, 0))

  }

}
