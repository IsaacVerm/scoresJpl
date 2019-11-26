#' @export
write_to_csv <- function(df, filename) {
  readr::write_csv(df, paste0(getwd(),"/data/",filename,".csv"))
}
