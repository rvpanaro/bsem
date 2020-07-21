#'  Run a shiny app
#'
#'  An introduction to the bsem package
#'  @details
#'  \describe{
#'    \item{signals}{this option is not allowed, it is only available in \code{\link[bsem]{sem}}}
#'    \item{stanfit}{S4 object of class stanfit}
#'  }
#'
#' @seealso \code{\link[bsem]{sem}}, \code{\link[bsem]{simdata}}, \code{\link[bsem]{arrayplot}}, \code{\link[bsem]{summary.bsem}}, \code{\link[bsem]{print.bsem}}
#' @examples
#'
#' \dontrun{
#' library("bsem")
#' runShiny()
#' }
#' @author RV Panaro
#'
#' @export runShiny
#'
runShiny <- function() {
  appDir <- system.file("shiny-examples", "shiny-bsem", package = "bsem")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bsem`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
