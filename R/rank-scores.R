#' @export
select_most_frequent_scores_by_season <- function(df_scores, nr_scores) {
  df_scores %>%
    dplyr::group_by(season, score) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(proportion = n / sum(n)) %>%
    dplyr::group_by(season) %>%
    dplyr::arrange(proportion) %>%
    dplyr::top_n(nr_scores)
}
