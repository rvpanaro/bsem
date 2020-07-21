#' Simulated data
#'
#' Function to generate artificial data from a structural equation model
#'
#' @export simdata
#' @param paths list referring to the inner model paths; a list of integers referring to the scores relationship; the jth first latent variable are explained
#' @param blocks list of integers in 1:ncol(data) indicating the manisfest variables corresponding to each block
#' @param exogenous, minimum color range (might cause blank spots if misspecified)
#' @param Nv number of endogenous variables in the database (default = 21)
#' @param Ne number of sample elements in the database (default = 30)
#' @param Nna number of missing observations in each database (default = 0)
#' @param save logical (default = FALSE); whether to save in the local filepath
#' @param sigma2 outer model error variances
#' @param beta inner paths regression coefficients
#' @param gamma0 inner exogenous intercept
#' @param gamma inner exogenous regression coefficients
#' @param tau2 inner exogenous error variance
#' @param name rdata name used if save = TRUE
#' @examples
#'
#' dt <- bsem::simdata()
#'
#' arrayplot(dt$real$alpha)
#' arrayplot(dt$real$alpha, colors = 0)
#' arrayplot(dt$real$alpha, colors = 1)
#' arrayplot(dt$real$alpha, -4, 4)
#'
#' @importFrom lattice levelplot panel.levelplot
#'
#'
#' @seealso \code{\link[bsem]{plot.bsem}}, \code{\link[bsem]{sem}}, \code{\link[bsem]{arrayplot}}, \code{\link[bsem]{summary.bsem}}, \code{\link[bsem]{print.bsem}}

simdata <-
  function(paths = list(3:4, 4:5),
           blocks = list(1:3, 4:7, 8:10, 11:16, 17:21),
           sigma2 = runif(Nv, 0.1, 0.9),
           exogenous = list(1:2),
           beta = list(c(1.0, -0.5), c(-1.0, 0.5)),
           gamma0 = list(c(1.5)),
           gamma = list(c(0.5, -1.0)),
           tau2 = list(c(0.49)),
           Nv = 21,
           Ne = 30,
           Nna = 0,
           save = FALSE,
           name  = "dt") {

    if(is.null(names(blocks))) names(blocks) <- paste0("F", 1:length(blocks))

    if(!is.null(exogenous)){
      names(exogenous) <- paste0("Y", 1:length(exogenous))

      aux = !unlist(exogenous) %in% 1:length(blocks)
      if(any(aux)){
        stop(sprintf("exogenous list latent variable[s] %s must be within block length range",
                     paste0(unlist(exogenous)[aux==T], collapse = ',')))
      }

      if(any(!lengths(gamma)) %in% lengths(exogenous))
        stop("lengths(gamma) not equal to lengths(exogenous)")

      if(length(exogenous) != sum(lengths(tau2)))
        stop("sum(lengths(tau2)) not equal to length(exogenous)")

      if(length(exogenous) != sum(lengths(gamma0)))
        stop("sum(lengths(gamma0)) not equal to length(exogenous)")
    }

    if(!is.null(paths)){
      names(paths) <- paste0("F", 1:length(paths))

      aux = !unlist(paths) %in% 1:length(blocks)
      if(any(aux)){
        stop(sprintf("paths list latent variable[s] %s must be within block length range",
                     paste0(unlist(paths)[aux==T], collapse = ',')))
      }

      if(any(!lengths(beta) %in% lengths(paths)))
        stop("lengths(beta) not equal to lengths(paths)")
    }

    aux = !unlist(blocks) %in% 1:Nv
    if(any(aux)){
      stop(sprintf("blocks list manifest variable[s] %s must be within 1:Nv",
                   paste0(unlist(blocks)[aux==T], collapse = ',')))
    }

    if(length(sigma2) != Nv)
      stop("length(sigma2) not equal to Nv")

    K <- length(blocks)
    lambda <- matrix(rnorm(K * Ne, 0, sqrt(1)),
      nrow = K,
      ncol = Ne
    )

    if (!is.null(paths)) {
      for (i in 1:length(paths)) {
        lambda[i, ] <- beta[[i]] %*% lambda[paths[[i]], ] + rnorm(Ne, 0, 1)
      }
    }
    else{
      beta = NULL
    }

    if (!is.null(exogenous)) {
      Y <- matrix(0,
                  nrow = length(exogenous),
                  ncol = Ne
      )

      for(i in 1:length(exogenous)){
        Y[i, ] <- gamma0[[i]] + gamma[[i]] %*% lambda[exogenous[[i]], ] + rnorm(Ne, 0, sqrt(tau2[[i]]))
      }
    }
    else{
      gamma0 = NULL
      gamma = NULL
      tau2 = NULL
    }

    erros <- array(0, c(Nv, Ne))
    alpha <- array(0, c(Nv, K))

    for (j in 1:Ne) {
      erros[, j] <- rnorm(1, 0, sqrt(sigma2))
    }

    for (k in 1:K) {
      nn <- lengths(blocks)[k]
      alpha[blocks[[k]], k] <- runif(nn, 0.5, 2) * sample(c(-1, 1), nn, replace = TRUE)
    }

    find = which(!1:nrow(alpha) %in% unlist(blocks))
    for(i in find){
      alpha[i, ] <- runif(K, 0.5, 2) * sign(rnorm(K))
    }

    X <- alpha %*% lambda + erros
    colnames(X) <- paste0("E", 1:Ne)

    if (Nna > 0) {
      X[sample(1:(Nv * Ne), Nna, replace = FALSE)] <- NA
    }

    real <- list(
      "alpha" = alpha, "lambda" = lambda,
      "sigma2" = sigma2, "beta" = beta, "tau2" = tau2,
      "gamma" = gamma, "gamma0" = gamma0
    )

    aux <- sign(alpha)
    S <- list()
    for (k in 1:K) {
      S[[k]] <- aux[blocks[[k]], k]
    }
    names(S) <- names(blocks)

    if(!is.null(exogenous)){
      data = t(rbind(Y, X))
      colnames(data) = c(paste0("Y", 1:nrow(Y)),
                         paste0("X", 1:nrow(X))
      )
    }
    else{
      data = t(X)
      colnames(data) = paste0("X", 1:nrow(X))
      real$gamma0 = NULL
      real$gamma = NULL
      real$tau2 = NULL
    }

    res = list(
      "data" = data %>% data.frame(),
      "real" = real,
      "blocks" = blocks,
      "signals" = S,
      "paths" = paths,
      "exogenous" = exogenous
      )

    if(save==TRUE){
      save(res, file = paste0(name,".rdata"))
    }

    return(res)
  }
