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

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("exogenous",
                                                        "prior_sigma2",
                                                        "par1_sigma2",
                                                        "par2_sigma2",
                                                        "priordist_beta",
                                                        "par1_beta",
                                                        "par2_beta",
                                                        "priordist_tau2",
                                                        "par1_tau2",
                                                        "par2_tau2",
                                                        "priordist_gamma",
                                                        "par1_gamma",
                                                        "par2_gamma",
                                                        "priordist_gamma",
                                                        "par1_gamma0",
                                                        "par2_gamma0",
                                                        "priordist_sigma2",
                                                        "priordist_gamma0"
                                                        ))
