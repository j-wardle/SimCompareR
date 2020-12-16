#' Summarize time and size of epidemic peak
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return summary
#' @export
#'

epidemic_peak <- function(simulation_data) {

  # function that takes dataframe of simulations and extracts:
  # a) time of peak(median? range? 95% range?)
  # b) number of cases at peak (median? 95% range?)

  peak <- simulation_data %>%
    dplyr::filter(variable == "infected") %>%
    dplyr::group_by(sim, patch) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::slice_head()

  peak_summary <- peak %>%
    dplyr::group_by(patch) %>%
    dplyr::summarise(dplyr::tibble(peak_time_median = stats::median(time),
                     peak_time_lo = stats::quantile(time, 0.025),
                     peak_time_hi = stats::quantile(time, 0.975),
                     peak_size_median = stats::median(value),
                     peak_size_lo = stats::quantile(value, 0.025),
                     peak_size_hi = stats::quantile(value, 0.975)))

  peak_summary


}
