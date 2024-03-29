#' @export
aggregate_scores_by_frequency <- function(df_scores) {
  df_scores %>%
    dplyr::group_by(score) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(proportion = n / sum(n)) # https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
}

#' @export
aggregate_average_goals_by_season <- function(df_scores) {
  df_scores %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      home = mean(goals_home),
      away = mean(goals_away)
    )
}

#' @export
average_goals_by_season_to_long <- function(average_goals_by_season) {
  average_goals_by_season %>%
    tidyr::gather(key = "venue",
                  value = "goals",
                  home,
                  away)
}

#' @export
aggregate_frequency_scores_by_season <- function(df_scores) {
  df_scores %>%
    dplyr::group_by(season, score) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(proportion = n / sum(n))
}

