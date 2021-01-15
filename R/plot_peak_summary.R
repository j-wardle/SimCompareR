#' Plot summary of time and size of epidemic peak
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return Plot summarising times and size of peaks in each patch
#' @importFrom ggplot2 ggplot aes
#' @export
#'

plot_peak_summary <- function(simulation_data, violin = FALSE) {


  ## Update this function later so that can specify if we want a violin plot instead of errorbar

  data_summary <- epidemic_peak(simulation_data)

  times <-  ggplot(data_summary) +
    ggplot2::geom_errorbar(aes(x = patch,
                               ymin = peak_time_lo,
                               ymax = peak_time_hi), width = 0.1) +
    ggplot2::geom_point(aes(x = patch, y = peak_time_median)) +
    ggplot2::xlab("Patch") +
    ggplot2::ylab("Time of peak (days)") +
    ggplot2::theme_classic()

  size <- ggplot(data_summary) +
    ggplot2::geom_errorbar(aes(x = patch,
                               ymin = peak_size_lo,
                               ymax = peak_size_hi), width = 0.1) +
    ggplot2::geom_point(aes(x = patch, y = peak_size_median)) +
    ggplot2::xlab("Patch") +
    ggplot2::ylab("Number of cases at peak \n(per 1,000 population)") +
    ggplot2::theme_classic()

  times + size

}
