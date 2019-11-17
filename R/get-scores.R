#' @export
get_seasons <- function() {
  c(
    "2006–07",
    "2007–08",
    "2008–09",
    "2009–10",
    "2010–11",
    "2011–12",
    "2012–13",
    "2013–14",
    "2014–15",
    "2015–16",
    "2016–17",
    "2017–18",
    "2018–19",
    "2019–20"
  )
}

get_path_league <- function(season) {
  league_by_season = list(
    "2006–07" = "Belgian_First_Division",
    "2007–08" = "Belgian_First_Division",
    "2008–09" = "Belgian_First_Division",
    "2009–10" = "Belgian_Pro_League",
    "2010–11" = "Belgian_Pro_League",
    "2011–12" = "Belgian_Pro_League",
    "2012–13" = "Belgian_Pro_League",
    "2013–14" = "Belgian_Pro_League",
    "2014–15" = "Belgian_Pro_League",
    "2015–16" = "Belgian_Pro_League",
    "2016–17" = "Belgian_First_Division_A",
    "2017–18" = "Belgian_First_Division_A",
    "2018–19" = "Belgian_First_Division_A",
    "2019–20" = "Belgian_First_Division_A"
  )

  league_by_season[[season]]
}

get_xpath_score_parsing <- function(season) {
  pattern_1 <-
    '//*[@id="Fixtures_and_results"]/../following-sibling::div[1]//td'
  pattern_2 <- '//*[@id="Results"]/../following-sibling::div[1]//td'
  pattern_3 <-
    '//*[@id="Results"]/../following-sibling::table[1]//td'

  pattern_by_season = list(
    "2006–07" = pattern_1,
    "2007–08" = pattern_2,
    "2008–09" = pattern_2,
    "2009–10" = pattern_2,
    "2010–11" = pattern_2,
    "2011–12" = pattern_2,
    "2012–13" = pattern_2,
    "2013–14" = pattern_2,
    "2014–15" = pattern_2,
    "2015–16" = pattern_3,
    "2016–17" = pattern_2,
    "2017–18" = pattern_2,
    "2018–19" = pattern_2,
    "2019–20" = pattern_2
  )

  pattern_by_season[[season]]
}

create_wiki_jpl_url <- function(season) {
  paste0("https://en.wikipedia.org/wiki/",
         season,
         "_",
         get_path_league(season))
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
parse_scores <- function(html, season) {
  html %>%
    rvest::html_nodes(xpath = get_xpath_score_parsing(season)) %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d+–\\d+") %>%
    .[!is.na(.)]
}

#' @export
get_scores <- function(seasons) {
  scores <- seasons %>%
    purrr::map(function(season) {
      season %>%
        create_wiki_jpl_url() %>%
        get_html_wiki_jpl() %>%
        parse_scores(season)
    })

  names(scores) <- seasons

  return(scores)
}
