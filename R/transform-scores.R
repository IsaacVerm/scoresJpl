#' @export
scores_to_df <- function(list_scores) {
  list_scores %>%
    purrr::imap_dfr(function(scores_season, season) {
      data.frame(season = season,
                 score = scores_season)
    })
}
