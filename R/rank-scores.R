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
