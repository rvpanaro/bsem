#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "shiny-bsem", package = "bsem")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bsem`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}