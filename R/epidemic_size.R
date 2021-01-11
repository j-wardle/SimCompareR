#' Plot histogram of final epidemic sizes
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return Plot summarising size of epidemic in each patch
#' @importFrom ggplot2 ggplot aes
#' @export

epidemic_size <- function(simulation_data) {

  final_size <- simulation_data %>%
    dplyr::filter(time == max(time) & variable == "recovered")

  plot <- ggplot2::ggplot(final_size) +
    ggplot2::geom_histogram(aes(value)) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Number of people infected") +
    ggplot2::ylab("Number of simulations") +
    ggplot2::facet_wrap(~ patch)

  table <- final_size %>%
    dplyr::group_by(patch) %>%
    dplyr::mutate(epidemic_flag = mmand::threshold(value, method = "kmeans")) %>%
    dplyr::summarise(prob_epidemic = sum(epidemic_flag) / dplyr::n())

  plot / gridExtra::tableGrob(table)

}
