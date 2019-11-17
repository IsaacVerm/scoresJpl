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
