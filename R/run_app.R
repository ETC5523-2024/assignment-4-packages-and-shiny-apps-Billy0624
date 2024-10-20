#' Launch the Shiny App
#'
#' This function launches the Shiny app for genderOlympics.
#'
#' @export

run_app <- function() {
  app_dir <- system.file("olympicApp", package = "genderOlympics")
  shiny::runApp(app_dir, display.mode = "normal")
}
