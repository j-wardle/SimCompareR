plot_attack_rate <- function(simulation_data) {

  attack_summary <- attack_rate(simulation_data)

  ggplot(attack_summary) +
    ggplot2::geom_errorbar(aes(x = patch,
                               ymin = attack_rate_lo,
                               ymax = attack_rate_hi), width = 0.1) +
    ggplot2::geom_point(aes(x = patch, y = attack_rate_median)) +
    ggplot2::xlab("Patch") +
    ggplot2::ylab("Attack rate") +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_classic()

}
