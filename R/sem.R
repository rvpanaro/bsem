#' Structural Equation Models (SEM) and particular cases via rstan interface
#'
#' Fits the SEM to specific data
#'
#' @title sem: The SEM Function
#' @param data  a mandatory data.frame object where the columns are variables and the rows are observations
#' @param paths  list referring to the inner model
#' @param blocks  list of column names (or integers in 1:ncol(data)) indicating the manisfest variables correpoding to each block;
#' @param signals  list referring to the signals of the factor loadings initial values; must be true: (length(signals) == length(blocks)) && (lengths(signals) == lengths(blocks))
#' @param rownames  refers to the observation vector of characters; must have nrow(data) lenght; defaults to rownames(data)
#' @param prior_specs  prior settings for the Bayesian approach; only `normal(0,...)` for beta and `inv_gamma` for sigma2 are temporarily available, beta is ignored if paths are not specific; next versions should include more options
#' @param pars  which parameters to be included in the outcome; options are any subset of default c("alpha", "lambda", "sigma2")
#' @param cores  number of core threads to be used
#' @param iter  number of iterations
#' @param chains  number of posterior chains
#' @param scaled  logical; indicates whether to center and scale the data; default FALSE
#' @param ...  further arguments passed to or from other methods
#' @export sem
#' @rdname sem
#' @importFrom rstan stan sampling
#' @importFrom coda  HPDinterval mcmc
#' @importFrom stats .getXlevels as.formula contrasts dbeta density dist formula median model.extract pbeta pchisq printCoefmat qnorm rlogis rnorm rweibull sd terms
#' @return  An object of class \code{sem}; a list of 14 to 16:
#' \describe{
#'    \item{stanfit}{S4 object of class stanfit}
#'    \item{posterior}{the list of posterior draws separate by chains}
#'    \item{model}{character; pointer to pre-defined stan model}
#'    \item{mean_alpha}{matrix of factor loadings posterior means}
#'    \item{mean_lambda}{matrix of factor scores posterior means}
#'    \item{mean_sigma2}{vector of error variances posterior means}
#'    \item{mean_beta}{vector of regression coefficients posterior means}
#'    \item{stats}{posterior descriptives statistics}
#'    \item{blocks}{list of blocks}
#'    \item{paths}{list of paths}
#'    \item{credint}{Highest posterior density intervals (HPD)}
#'    \item{h}{vector of communalities}
#'    \item{PVTE}{vector of total variance proportions}
#'    \item{AFR2}{adjusted coefficient of determination}
#'    \item{SQE}{explained sums of squares}
#'    \item{SQT}{total sums of squares}
#' }
#' @examples
#' data("set1")
#'
#' fit1 <- sem(
#'   data = set1$set, blocks = set1$blocks,
#'   chains = 1
#' )
#' summary(fit1)
#' \dontrun{
#' data("set2")
#'
#' fit2 <- sem(
#'   data = set2$set, blocks = set2$blocks,
#'   chains = 4, iter = 2500, warmup = 1000
#' )
#' summary(fit2)
#' }
#'

sem <-
  function(data,
           paths,
           blocks,
           exogenous,
           signals,
           row_names = rownames(data),
           prior_specs = list(
             beta = c("normal(0,1)"),
             sigma2 = c("inv_gamma(2.1, 1.1)"),
             gamma0 = c("normal(0,1)"),
             gamma = c("normal(0,1)"),
             tau2 = c("inv_gamma(2.1, 1.1)")
           ),
           cores = parallel::detectCores(),
           pars = c("alpha", "lambda", "sigma2"),
           iter = 2000,
           chains = 4,
           scaled = FALSE,
           ...) {
    ifelse(scaled, X <-
      t(scale(data)), X <-
      t(data)) # format: lines = R-space (variables) and columns= Q-space (observations)
    if (is.null(row_names)) {
      row_names <- paste0("X", 1:nrow(data))
    }
    if (is.null(colnames(data))) {
      colnames(data) <- paste0("V", 1:ncol(data))
    }
    if (is.null(names(blocks))) {
      names(blocks) <- paste0("F", 1:length(blocks))
    }
    if (!missing(exogenous)) {
      if (is.null(names(exogenous))) {
        names(exogenous) <- colnames(data)[1:length(exogenous)]
        warning(
          gettextf(
            "underspecification;
            names(exogenous) is missing: the first %s [column] variables became exogenous",
            length(exogenous)
          )
        )
      }
    }
    if (missing(signals)) {
      warning("underspecification;
              signals not specified: random initial values assigned for alpha")
    }

    # missing data
    idob <- which(is.na(X) == FALSE, arr.ind = TRUE)

    Nob <- nrow(idob) # total number of observations (complete + incomplete)
    idna <- which(is.na(X) == TRUE, arr.ind = TRUE)
    Nna <- nrow(idna) # total number of missing observations

    X[idna] <- apply(X, 1, mean, na.rm = TRUE)[idna[, 1]] # replace NAs by means

    Nv <- nrow(X) # total number of variables
    Ne <- ncol(X) # total number of observations
    K <- length(blocks)

    B <- list()
    for (i in 1:K) {
      if (class(blocks[[i]]) == "character") {
        aux <- as.numeric(unlist(blocks) %in% colnames(data))

        if (any(aux == 0)) {
          stop(gettextf(
            "argument misspecification;
                        argument %s not matched",
            unlist(blocks)[aux == 0]
          ))
        }

        B[[i]] <-
          which(colnames(data) %in% blocks[[i]]) # search by variable names
      }
      else if (class(blocks[[i]]) %in% c("numeric", "integer", "vector")) {
        if (!all(blocks[[i]] == floor(blocks[[i]]))) {
          stop(gettextf(
            "blocks misspecification;
            %s must be integer",
            paste0(i, collapse = ", ")
          ))
        }
        aux <- !blocks[[i]] %in% 1:ncol(data)
        if (any(aux)) {
          stop(gettextf(
            "blocks misspecification;
            there is no columns %s in data.frame",
            paste0(blocks[[i]][aux == T], collapse = ", ")
          ))
        }

        B[[i]] <- blocks[[i]]
        blocks[[i]] <- colnames(data)[blocks[[i]]]
      }
      else {
        stop("blocks misspecification;
             blocks  must be a list of character or numeric")
      }
    }

    if (!missing(exogenous)) {
      v <- array(0.0001, c(Nv - length(exogenous), K))
    }
    else {
      v <- array(0.0001, c(Nv, K))
    }


    for (k in 1:K) {
      if (k %in% 1:length(B)) {
        v[B[[k]], k] <- 9
      }

      v[-unlist(B), ] <- 9
    } # vcov matrix for factor loadings

    handler1()
    handler2()

    if (!exists("init")) {
      handler3(missing(signals))
    }

    standata <-
      list(
        X = X,
        Nv = Nv,
        Ne = Ne,
        K = K,
        v = v,
        dsigma2 = priordist_sigma2,
        a = par1_sigma2,
        b = par2_sigma2,
        dbeta = priordist_beta,
        m = par1_beta,
        s = par2_beta,
        dtau2 = priordist_tau2,
        at = par1_tau2,
        bt = par2_tau2,
        dgamma = priordist_gamma,
        mg = par1_gamma,
        sg = par2_gamma,
        dgamma0 = priordist_gamma0,
        mg0 = par1_gamma0,
        sg0 = par2_gamma0,
        idob = idob,
        idna = idna,
        Nob = Nob,
        Nna = Nna
      )

    ## stanfit
    ## ----
    if (Nna == 0) {
      ## without missing variables

      if (missing(paths)) {
        if (missing(exogenous)) {
          ## blocks

          stanfit <-
            suppressWarnings(
              sampling(
                stanmodels$factorial,
                data = standata,
                pars = pars,
                verbose = FALSE,
                init = init,
                chains = chains,
                iter = iter,
                cores = cores,
                ...
              )
            )
        }
        else {
          ## blocks + exogenous
          handler5()
          stanfit <-
            suppressWarnings(
              sampling(
                stanmodels$factorialEX,
                data = standata,
                pars = pars,
                verbose = FALSE,
                init = init,
                chains = chains,
                iter = iter,
                cores = cores,
                ...
              )
            )
        }
      }
      else {
        handler4()

        if (missing(exogenous)) {
          ## blocks + paths

          stanfit <- suppressWarnings(
            sampling(
              stanmodels$sem,
              data = standata,
              pars = pars,
              verbose = FALSE,
              init = init,
              chains = chains,
              iter = iter,
              cores = cores,
              ...
            )
          )
        }
        else {
          ## blocks + paths + exogenous
          handler5()

          stanfit <- suppressWarnings(
            sampling(
              stanmodels$semEX,
              data = standata,
              pars = pars,
              verbose = FALSE,
              init = init,
              chains = chains,
              iter = iter,
              cores = cores,
              ...
            )
          )
        }
      }
    }
    else {
      pars <- c(pars, "Xna")

      ## with missing variable fit
      for (i in 1:chains) {
        init[[i]]$Xna <- array(0, Nna)
      }

      if (missing(paths)) {
        if (missing(exogenous)) {
          ## blocks + missing
          stanfit <-
            suppressWarnings(
              sampling(
                stanmodels$factorialNA,
                data = standata,
                pars = pars,
                verbose = FALSE,
                init = init,
                chains = chains,
                iter = iter,
                cores = cores,
                ...
              )
            )
        }
        else {
          handler5()
          ## blocks + missing + exogenous
          stanfit <-
            suppressWarnings(
              sampling(
                stanmodels$factorialNAEX,
                data = standata,
                pars = pars,
                verbose = FALSE,
                init = init,
                chains = chains,
                iter = iter,
                cores = cores,
                ...
              )
            )
        }
      }
      else {
        ## with inner model
        handler4()

        if (missing(exogenous)) {
          ## blocks + paths + missing
          stanfit <- suppressWarnings(
            sampling(
              stanmodels$semNA,
              data = standata,
              pars = pars,
              verbose = FALSE,
              init = init,
              chains = chains,
              iter = iter,
              cores = cores,
              ...
            )
          )
        }
        else {
          handler5()
          ## blocks + paths + missing + exogenous
          stanfit <- suppressWarnings(
            sampling(
              stanmodels$semNAEX,
              data = standata,
              pars = pars,
              verbose = FALSE,
              init = init,
              chains = chains,
              iter = iter,
              cores = cores,
              ...
            )
          )
        }
      }
    }
    cat("\nExtracting posterior samples, please wait...\n")
    samples <-
      list(
        alpha = rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "alpha",
          inc_warmup = FALSE
        ),
        lambda = rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "lambda",
          inc_warmup = FALSE
        ),
        sigma2 = rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "sigma2",
          inc_warmup = FALSE
        )
      )
    if (stanfit@model_name %in% c("factorialNA", "semNA", "factorialNAEX", "semNAEX")) {
      samples$Xna <-
        rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "Xna",
          inc_warmup = FALSE
        )
    }
    if (stanfit@model_name %in% c("sem", "semNA", "semEX", "semNAEX")) {
      samples$beta <-
        rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "beta",
          inc_warmup = FALSE
        )
    }
    if (stanfit@model_name %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX")) {
      samples$gamma0 <-
        rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "gamma0",
          inc_warmup = FALSE
        )

      samples$gamma <-
        rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "gamma",
          inc_warmup = FALSE
        )

      samples$tau2 <-
        rstan::extract(
          stanfit,
          permuted = FALSE,
          pars = "tau2",
          inc_warmup = FALSE
        )
    }

    handler6()

    invisible(capture.output(aux <-
      lapply(samples, function(x) {
        apply(x, 3, rstan::monitor, warmup = 0)
      })))

    stats <- matrix(unlist(aux), ncol = 22, byrow = TRUE)
    colnames(stats) <- colnames(aux[[1]][[1]])
    rownames(stats) <- unlist(lapply(aux, names))

    output <- list(
      stanfit = stanfit,
      posterior = samples,
      model = stanfit@model_name,
      mean_alpha = matrix(stats[startsWith(rownames(stats), "alpha"), "mean"], ncol = K),
      mean_lambda = matrix(stats[startsWith(rownames(stats), "lambda"), "mean"], nrow = K),
      mean_sigma2 = stats[startsWith(rownames(stats), "sigma2"), "mean"],
      stats = stats
    )

    if (stanfit@model_name %in% c("factorialNA", "semNA", "factorialNAEX", "semNAEX")) {
      output$Xna <- stats[startsWith(rownames(stats), "Xna"), "mean"]
    }
    if (stanfit@model_name %in% c("sem", "semNA", "semEX", "semNAEX")) {
      output$mean_beta <- stats[startsWith(rownames(stats), "beta"), "mean"]
    }
    if (stanfit@model_name %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX")) {
      output$mean_tau2 <- stats[startsWith(rownames(stats), "tau2"), "mean"]
      output$mean_gamma <- stats[startsWith(rownames(stats), "gamma"), "mean"]
    }

    if (!missing(blocks)) {
      output$blocks <- blocks
    }
    if (!missing(paths)) {
      output$paths <- paths
    }
    if (!missing(exogenous)) {
      output$exogenous <- exogenous
    }

    # HPD intervals
    output$credint <-
      list(
        alpha = t(apply(samples$alpha, 3, function(x) {
          HPDinterval(mcmc(as.vector(x)))
        })),
        lambda = t(apply(samples$lambda, 3, function(x) {
          HPDinterval(mcmc(as.vector(x)))
        })),
        sigma2 = t(apply(samples$sigma2, 3, function(x) {
          HPDinterval(mcmc(as.vector(x)))
        }))
      )
    if (stanfit@model_name %in% c("factorialNA", "semNA", "factorialNAEX", "semNAEX")) {
      output$credint$Xna <-
        t(apply(samples$Xna, 3, function(x) {
          HPDinterval(mcmc(as.vector(
            x
          )))
        }))
    }
    if (stanfit@model_name %in% c("sem", "semNA", "semEX", "semNAEX")) {
      output$credint$beta <-
        t(apply(samples$beta, 3, function(x) {
          HPDinterval(mcmc(as.vector(
            x
          )))
        }))
    }
    if (stanfit@model_name %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX")) {
      output$credint$gamma0 <-
        t(apply(samples$gamma0, 3, function(x) {
          HPDinterval(mcmc(as.vector(
            x
          )))
        }))

      output$credint$gamma <-
        t(apply(samples$gamma, 3, function(x) {
          HPDinterval(mcmc(as.vector(
            x
          )))
        }))
      output$credint$tau2 <-
        t(apply(samples$tau2, 3, function(x) {
          HPDinterval(mcmc(as.vector(
            x
          )))
        }))
    }

    # output$credint <- credint
    output$h <-
      diag(output$mean_alpha %*% t(output$mean_alpha)) ## comunalities
    output$PVTE <- 100 * output$h / (output$h + output$mean_sigma2) ## proportion of total variability

    output$SQE <-
      sum(((
        output$mean_alpha %*% output$mean_lambda
      ))^2)
    output$SQT <- sum((standata$X - mean(standata$X))^2)
    output$AFR2 <-
      100 * (1 - output$SQE / output$SQT) # AFR2 = adapted to factor analisys R2

    if (Nna > 0) {
      output$idna <- idna
    }

    ifelse(missing(exogenous),
      rownames(output$mean_alpha) <- colnames(data),
      rownames(output$mean_alpha) <- colnames(data)[-standata$idex]
    )


    colnames(output$mean_alpha) <- names(blocks)
    rownames(output$mean_lambda) <- names(blocks)

    colnames(output$mean_lambda) <- row_names

    ifelse(missing(exogenous),
      names(output$mean_sigma2) <- colnames(data),
      names(output$mean_sigma2) <- colnames(data)[-standata$idex]
    )

    cat("\nDone!\n")
    class(output) <- "bsem"

    return(output)
  }
