epidemic_peak <- function(simulation_data) {

  # function that takes dataframe of simulations and extracts:
  # a) time of peak(median? range? 95% range?)
  # b) number of cases at peak (median? 95% range?)

  peak <- simulation_data %>%
    filter(variable == "infected") %>%
    group_by(sim, patch) %>%
    arrange(desc(value)) %>%
    slice_head()

  peak_summary <- peak %>%
    group_by(patch) %>%
    summarise(tibble(peak_time_median = median(time),
                     peak_time_lo = quantile(time, 0.025),
                     peak_time_hi = quantile(time, 0.975),
                     peak_size_median = median(value),
                     peak_size_lo = quantile(value, 0.025),
                     peak_size_hi = quantile(value, 0.975)))

  peak_summary


}
