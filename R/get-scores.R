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