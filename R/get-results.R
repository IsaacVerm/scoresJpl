create_wiki_jpl_url <- function(season) {
  paste0("https://en.wikipedia.org/wiki/",
         season,
         "_Belgian_Pro_League")
}

get_html_wiki_jpl <- function(url) {
  response <- httr::GET(url)
  httr::content(response)
}

parse_results <- function(html) {

}
