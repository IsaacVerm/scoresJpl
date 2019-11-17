create_wiki_jpl_url <- function(season) {
  start_year <- substr(season, 1, 4)
  end_year <- substr(season, 6, 8)

  if (start_year > 2009) {
    league = "_Belgian_Pro_League"
  } else if (start_year > 2005) {
    league = "_Belgian_First_Division"
  } else {
    stop("You're trying to create the url for a season without results data.")
  }

  paste0("https://en.wikipedia.org/wiki/",
         start_year,
         "-",
         end_year,
         league)
}

get_html_wiki_jpl <- function(url) {
  xml2::read_html(url)
}

#' Parses the scores of the html wiki season page.
#'
#' \code{parse_scores}
#'
#' The xpath works like this:
#'
#' 1) Look for a Results id.
#'
#' 2) Go to the parent. I'm not specific here about what the parent should be like because it can be h2, h3, ...
#'
#' 3) Look for the next following sibling. Next because [1] is specified.
#'
#' 4) Get each cell of the table.
parse_scores <- function(html) {
  html %>%
    rvest::html_nodes(xpath = '//*[@id="Results"]/../following-sibling::div[1]/div/table//td') %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d+–\\d+")
}

get_scores <- function(seasons) {
  seasons %>%
    purrr::map(function(season) {
      season %>%
        create_wiki_jpl_url() %>%
        get_html_wiki_jpl() %>%
        parse_scores()
    })
}
