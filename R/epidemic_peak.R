#' Summarize time and size of epidemic peak
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return summary
#' @importFrom rlang .data
#' @export
#'

epidemic_peak <- function(simulation_data, filter = TRUE) {

  # function that takes dataframe of simulations and extracts:
  # a) time of peak(median? range? 95% range?)
  # b) number of cases at peak (median? 95% range?)

  if(filter == TRUE) {
    simulation_data <- epidemic_anywhere(simulation_data) %>%
      dplyr::filter(patch_epi == 1)
  }

  compartments <- c("susceptible", "exposed", "infected", "recovered")

  sim_population <- simulation_data %>%
    dplyr::filter(variable %in% compartments) %>%
    dplyr::group_by(sim, patch, time) %>%
    dplyr::mutate(population = sum(value))

  peak <- sim_population %>%
    dplyr::filter(variable == "infected") %>%
    dplyr::group_by(sim, patch) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::slice_head() %>%
    dplyr::mutate(infection_rate = 1000 * value / population)

  peak_summary <- peak %>%
    dplyr::group_by(patch) %>%
    dplyr::summarise(dplyr::tibble(peak_time_median = stats::median(time),
                     peak_time_lo = stats::quantile(time, 0.025),
                     peak_time_hi = stats::quantile(time, 0.975),
                     peak_size_median = stats::median(infection_rate),
                     peak_size_lo = stats::quantile(infection_rate, 0.025),
                     peak_size_hi = stats::quantile(infection_rate, 0.975)))

  peak_summary$patch <- forcats::as_factor(peak_summary$patch)

  peak_summary


}
