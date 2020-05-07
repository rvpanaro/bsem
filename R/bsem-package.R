#' The 'bsem' package.
#'
#' @description The bsem package allows Bayesian analysis for particular cases of structural equation models (SEMs) based on rstan integration. Examples include confirmatory factor analysis and confirmatory SEM. The full SEM model (outer and inner models), enables the evaluation of user-defined latent variables along with the analysis of established linear relationships among the latent scores.
#'
#' @docType package
#' @name bsem-package
#' @aliases bsem
#' @useDynLib bsem, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#' Mayrink, V. D., & Lucas, J. E. (2013). Sparse latent factor models with interactions: Analysis of gene expression data. The Annals of Applied Statistics, 7(2), 799-822.
#'
#' Mayrink, V. D., & Lucas, J. E. (2015). Bayesian factor models for the detection of coherent patterns in gene expression data. Brazilian Journal of Probability and Statistics, 29(1), 1-33.
#'
NULL
