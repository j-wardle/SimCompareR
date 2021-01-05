#' Summarize time and size of epidemic peak
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return summary
#' @importFrom rlang .data
#' @export
#'

epidemic_peak <- function(simulation_data) {

  # function that takes dataframe of simulations and extracts:
  # a) time of peak(median? range? 95% range?)
  # b) number of cases at peak (median? 95% range?)

  # Use of .data in following code doesn;t seem to work
  # peak <- simulation_data %>%
  #   dplyr::filter(.data$variable == "infected") %>%
  #   dplyr::group_by(.data$sim, .data$patch) %>%
  #   dplyr::arrange(desc(.data$value)) %>%
  #   dplyr::slice_head()

  # peak_summary <- peak %>%
  #   dplyr::group_by(.data$patch) %>%
  #   dplyr::summarise(dplyr::tibble(peak_time_median = stats::median(.data$time),
  #                                  peak_time_lo = stats::quantile(.data$time, 0.025),
  #                                  peak_time_hi = stats::quantile(.data$time, 0.975),
  #                                  peak_size_median = stats::median(.data$value),
  #                                  peak_size_lo = stats::quantile(.data$value, 0.025),
  #                                  peak_size_hi = stats::quantile(.data$value, 0.975)))

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

  peak_summary$patch <- forcats::as_factor(peak_summary$patch)

  peak_summary


}
