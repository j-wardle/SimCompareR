#' Summarize attack rates in simulations
#'
#' @param simulation_data Simulated epidemics from metapopulation SEIR model
#'
#' @return Summary of the attack rates in simulations where epidemics took place
#' @export
#'


attack_rate <- function(simulation_data) {

  compartments <- c("susceptible", "exposed", "infected", "recovered")

  attack_population <- simulation_data %>%
    dplyr::filter(time == max(time) & variable %in% compartments) %>%
    dplyr::group_by(sim, patch) %>%
    dplyr::mutate(population = sum(value)) %>%
    dplyr::filter(variable == "recovered")

  attack_rates <- attack_population %>%
    dplyr::ungroup() %>%
    dplyr::group_by(patch) %>%
    # dplyr::mutate(epidemic_flag = mmand::threshold(value, method = "kmeans")) %>%
    # dplyr::filter(epidemic_flag == 1) %>%
    dplyr::mutate(attack_rate = value / population) %>%
    dplyr::summarise(dplyr::tibble(
      attack_rate_median = stats::median(attack_rate),
      attack_rate_lo = stats::quantile(attack_rate, 0.025),
      attack_rate_hi = stats::quantile(attack_rate, 0.975)
    ))

  attack_rates$patch <- forcats::as_factor(attack_rates$patch)

  attack_rates

}
