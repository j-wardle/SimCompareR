plot_attack_rate <- function(simulation_data, filter = TRUE) {

  # attack_summary <- attack_rate(simulation_data)
  #
  # ggplot(attack_summary) +
  #   ggplot2::geom_errorbar(aes(x = patch,
  #                              ymin = attack_rate_lo,
  #                              ymax = attack_rate_hi), width = 0.1) +
  #   ggplot2::geom_point(aes(x = patch, y = attack_rate_median)) +
  #   ggplot2::xlab("Patch") +
  #   ggplot2::ylab("Attack rate") +
  #   ggplot2::ylim(c(0,1)) +
  #   ggplot2::theme_classic()
  #
  #
  #Violin plot??
  if(filter == TRUE) {
    simulation_data <- epidemic_anywhere(simulation_data) %>%
      dplyr::filter(patch_epi == 1)
  }

  compartments <- c("susceptible", "exposed", "infected", "recovered")

  attack_population <- simulation_data %>%
    dplyr::filter(time == max(time) & variable %in% compartments) %>%
    dplyr::group_by(sim, patch) %>%
    dplyr::mutate(population = sum(value)) %>%
    dplyr::filter(variable == "recovered")

  attack_rates <- attack_population %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sim, patch) %>%
    # dplyr::mutate(epidemic_flag = mmand::threshold(value, method = "kmeans")) %>%
    # dplyr::filter(epidemic_flag == 1) %>%
    dplyr::mutate(attack_rate = value / population)

  attack_rates$patch <- forcats::as_factor(attack_rates$patch)

  ggplot(attack_rates, aes(x = patch, y = attack_rate)) +
    ggplot2::geom_violin(trim = FALSE) +
    ggplot2::stat_summary(fun = median, geom = "point", size = 2, colour = "red") +
    # ggplot2::geom_boxplot(width = 0.05) +
    # ggplot2::geom_point(aes(x = patch, y = attack_rate_median)) +
    ggplot2::xlab("Patch") +
    ggplot2::ylab("Attack rate") +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_classic()

}
