#' @export
get_most_frequent_scores <- function(scores_by_frequency, nr_scores) {
  scores_by_frequency %>%
    dplyr::arrange(proportion) %>%
    dplyr::top_n(nr_scores) %>%
    dplyr::pull(score)
}

#' @export
select_most_frequent_scores_by_season <- function(frequency_scores_by_season, most_frequent_scores) {
  frequency_scores_by_season %>%
    dplyr::group_by(season) %>%
    dplyr::filter(score %in% most_frequent_scores)
}

#' @export
get_outlier_index <- function(most_frequent_scores_by_season, scores_by_frequency) {
  # total games here is total of number of games with frequent scores
  total_games_by_season <- most_frequent_scores_by_season %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(total = sum(n))

  most_frequent_scores_by_season %>%
    dplyr::left_join(total_games_by_season, by = "season") %>%
    dplyr::mutate(weighted_proportion = n / total) %>%
    dplyr::left_join(scores_by_frequency, by = "score") %>%
    dplyr::mutate(weighted_proportion_difference = abs(proportion.x - proportion.y) * weighted_proportion) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(average_proportion_difference = mean(weighted_proportion_difference))
}
