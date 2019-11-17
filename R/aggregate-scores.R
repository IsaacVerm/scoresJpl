#' @export
aggregate_scores_by_frequency <- function(df_scores) {
  df_scores %>%
    dplyr::group_by(score) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(proportion = n / sum(n)) # https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
}
