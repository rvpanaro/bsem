#' Data set 1; generated from a factor analysis model
#'
#' @docType data
#'
#' @usage data(set1)
#'
#' @format A list of 3 objects contaning the data set, the real (true) values and the blocks of variables that manifest the latent scores
#' \describe{
#'    \item{set}{the artificial data set; \code{double[20x100]} }
#'    \item{real}{a list of 3 containing the true values of the parameters; alpha = loadings, lambda = scores, sigma2 = error variance}
#'    \item{blocks}{list of 5 integer arrays describing the relationship between observed and unobserved variables}
#' }
#'
#' @keywords datasets set1 set2
#'
#' @examples
#' data(set1)
"set1"

#' Data set 2; generated from a structural equation model
#'
#' @docType data
#'
#' @usage data(set2)
#'
#' @format A list of 5 objects contaning the data set, the real (true) values, the blocks of variables that manifest the latent scores, the list of loadings' signals (magnitude's direction) and the list of linear relationships among latent varibles
#' \describe{
#'    \item{set}{the artificial data set; \code{double[20x100]} }
#'    \item{real}{a list of 5 containing the true values of the parameters; alpha = loadings, lambda = scores, sigma2 = error variance; beta = regression coeficients; tau = regression variance}
#'    \item{blocks}{list of 5 integer arrays describing the relationship between observed and unobserved variables}
#'    \item{signals}{list of 5 integer arrays describing the block signals}
#'    \item{paths}{list of 3 integer arrays describing three linear regressions among unobserved variables}
#' }
#'
#' @keywords datasets set1 set2
#'
#' @examples
#' data(set2)
"set2"
