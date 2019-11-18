#' @export
plot_proportion_scores <- function(scores_by_frequency) {
  ggplot2::ggplot(data = scores_by_frequency,
                  ggplot2::aes(x = reorder(score, -proportion),
                               y = round(proportion * 100, 2))) +
    ggplot2::geom_col() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = "Scores naargelang procentueel voorkomen",
                  x = "score",
                  y = "procentueel voorkomen")
}

#' @export
plot_average_goals_by_season <- function(long_average_goals_by_season) {
  ggplot2::ggplot(data = long_average_goals_by_season,
                  ggplot2::aes(x = season,
                               y = goals,
                               group = goals_home_or_away,
                               fill = goals_home_or_away)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = "gemiddeld aantal doelpunten per wedstrijd naar seizoen",
         x = "seizoen",
         y = "gemiddeld aantal doelpunten per wedstrijd",
         fill = "thuis-en uitdoelpunten") +
    ggplot2::scale_fill_manual(values = c("#999999", "#F8766D"),
                               labels = c("uit","thuis"))
}

#' @export
plot_most_frequent_scores_by_season <- function(most_frequent_scores_by_season) {
  ggplot2::ggplot(data = most_frequent_scores_by_season,
                  ggplot2::aes(x = score,
                               y = proportion)) +
    ggplot2::facet_wrap(~season)
}
