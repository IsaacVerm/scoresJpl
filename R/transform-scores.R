#' @export
scores_to_df <- function(list_scores) {
  list_scores %>%
    purrr::imap_dfr(function(scores_season, season) {
      data.frame(season = season,
                 score = scores_season)
    })
}

#' @export
add_goals_home <- function(df_scores) {
  df_scores %>%
    dplyr::mutate(goals_home = stringr::str_extract_all(score,
                                                        "\\d+",
                                                        simplify = TRUE)[, 1] %>% as.integer)
}

#' @export
add_goals_away <- function(df_scores) {
  df_scores %>%
    dplyr::mutate(goals_away = stringr::str_extract_all(score,
                                                        "\\d+",
                                                        simplify = TRUE)[, 2] %>% as.integer)
}

#' @export
add_goal_difference <- function(df_scores) {
  df_scores %>%
    dplyr::mutate(goal_difference = goals_home - goals_away)
}
