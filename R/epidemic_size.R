#' Plot histogram of final epidemic sizes
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return Plot summarising size of epidemic in each patch
#' @importFrom ggplot2 ggplot aes
#' @export

epidemic_size <- function(simulation_data) {

  final_size <- epidemic_anywhere(simulation_data) %>%
    dplyr::filter(time == max(time) & variable == "recovered")

  final_size$any_epi <- as.factor(final_size$any_epi)

  plot <- ggplot2::ggplot(final_size, aes(x = value, fill = any_epi, col = any_epi)) +
    ggplot2::geom_histogram(position = "identity", alpha = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Number of people infected") +
    ggplot2::ylab("Number of simulations") +
    ggplot2::facet_wrap(~ patch)

  table <- final_size %>%
    dplyr::group_by(patch) %>%
    dplyr::summarise(prob_epidemic = sum(patch_epi) / dplyr::n())

  plot / gridExtra::tableGrob(table)

}
