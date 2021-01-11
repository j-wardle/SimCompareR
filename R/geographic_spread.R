geographic_spread <- function(simulation_data) {

  spread <- simulation_data %>%
    dplyr::filter(variable == "infected") %>%
    dplyr::mutate(patch_infected = ifelse(value > 0, 1, 0)) %>%
    dplyr::group_by(sim, time) %>%
    dplyr::summarise(patch_spread = sum(patch_infected)) %>%
    dplyr::group_by(sim, patch_spread) %>%
    dplyr::slice_min(time, n = 1) %>%
    dplyr::filter(patch_spread != 0)

  spread$patch_spread <- forcats::as_factor(spread$patch_spread)

  spread_over_time <- ggplot(spread) +
    ggplot2::geom_line(aes(x = time, y = patch_spread, group = sim)) +
    ggplot2::xlab("Time (days)") +
    ggplot2::ylab("Number of patches infected") +
    ggplot2::theme_classic()

  spread$patch_spread <- as.numeric(spread$patch_spread)

  patches_affected <- spread %>%
    dplyr::group_by(sim) %>%
    dplyr::summarise(patches_affected = max(patch_spread))

  patches_affected_plot <- ggplot(patches_affected) +
    ggplot2::geom_histogram(aes(patches_affected)) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Number of patches infected") +
    ggplot2::ylab("Number of simulations")

  patches_affected_plot + spread_over_time


}
