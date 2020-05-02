#' Structural Equation Models (SEM) and particular cases via rstan interface
#'
#' Fits the SEM to specific data
#'
#' @title sem: The SEM Function
#' @param data a mandatory data.frame object where the columns are variables and the rows are observations
#' @param paths list referring to the inner model
#' @param blocks list of column names indicating the manisfest variables correpoding to each block
#' @param priors prior settings for the Bayesian approach; `normal` or `cauchy` for beta; `gamma`, `inv_gamma` or `lognormal` for gamma (BP coefficients)
#' @param scaled logical; indicates whether to center and scale the data
#' @param ... further arguments passed to or from other methods
#' @param cores number of core threads to use
#' @return An object of class \code{sem}
#' @export sem
#' @rdname sem
#' @importFrom rstan stan sampling
#' @importFrom loo waic loo
#' @importFrom coda  HPDinterval
#' @importFrom stats .getXlevels as.formula contrasts dbeta density dist formula median model.extract pbeta pchisq printCoefmat qnorm rlogis rnorm rweibull sd terms
#'

sem <-
  function(data,
           paths,
           blocks,
           signals,
           row_names = rownames(data),
           priors = list(beta = c("normal(0,1)"),
                         noise = c("gamma(2.1, 1.1)")),
           cores =  parallel::detectCores(),
           pars = c("alpha", "lambda", "sigma2", "Xna"),
           iter = 4000,
           chains = 4,
           scaled = TRUE,
           ...){

          q = 1
    ifelse(scaled, X <- t(scale(data)), X <- t(data)) # format: lines = R-space (variables) and columns= Q-space (observations)
    if(is.null(row_names)){row_names <- paste0('obs',1:nrow(data))}
    if(is.null(colnames(data))){colnames(data) <- paste0('var',1:ncol(data))}
    if(is.null(names(blocks))){names(blocks) <- paste0('latent', 1:length(blocks))}

    # missing data
    idob = which(is.na(X) == FALSE, arr.ind = TRUE)
    Nob = nrow(idob) # total number of observations (complete + incomplete)
    idna = which(is.na(X) == TRUE, arr.ind = TRUE)
    Nna = nrow(idna) # total number of missing observations

    X[idna] = apply(X, 1, mean, na.rm = TRUE)[idna[,1]] # replace NAs by means

    Nv = nrow(X) # total number of variables
    Ne = ncol(X) # total number of observations
    K <- length(blocks)

    B <- list()
      for(i in 1:K){
        if(class(blocks[[i]]) == "character"){

          aux <- as.numeric(unlist(blocks) %in% colnames(data))

          if(any(aux == 0)){stop(gettextf("Argument %s not matched", unlist(blocks)[aux==0]))}

          B[[i]] <- which(colnames(data) %in% blocks[[i]]) # search by variable names
        }
        else if(class(blocks[[i]]) %in%  c("numeric", "integer", "vector")){
          if(!all(blocks[[i]] == floor(blocks[[i]]))){
            stop('the', paste0(i,"th"), "block element must be integer")
          }
          B[[i]] <- blocks[[i]]
          blocks[[i]] <- colnames(data)[blocks[[i]]]
        }
        else{
          stop("blocks elements must be either character or numeric")
        }
      }

    v = array(0.0001, c(Nv,K))

    for(k in 1:K){
      if(k %in%  1:length(B)){
        v[B[[k]], k] <- 9
      }

      v[-unlist(B), ] <- 9
    } # vcov matrix for factor loadings

    handler1(); handler2;

    aux <- unlist(B)

    if(any(aux > ncol(data)))
      stop(gettextf("Manifest variable in column %s does not exist", aux[aux > ncol(data)]))

    if(!exists("init")){handler3(missing(signals))}

    a = par1_noise[1]; b = par2_noise[1]

    standata <- list(X = X, Nv = Nv, Ne = Ne, K = K, v = v, a = a, b = b,
                     idob = idob, idna = idna, Nob = Nob, Nna = Nna)

    ## stanfit
    ## ----

    if(Nna == 0){    ## without missing variables
      pars <- pars[!pars %in% "Xna"]

      if(missing(paths)){   ## without inner model
        stanfit <- suppressWarnings(sampling(stanmodels$factorial,
                            data = standata, pars = pars,
                            verbose = FALSE, init = init,
                            chains = chains, iter = iter,
                            cores = cores, ...))
      }
      else{    ## with inner model
        handler4()
        pars <- c(pars, "beta")

        standata$Ny = Ny
        standata$idy = array(idy)
        standata$idyi = array(idyi)
        standata$nbeta = array(nbeta)
        standata$idlamb = array(idlamb)

        stanfit <- suppressWarnings(sampling(stanmodels$sem,
                            data = standata, pars = pars,
                            verbose = FALSE, init = init,
                            chains = chains, iter = iter,
                            cores = cores, ...))
      }
    }
    else{    ## missing variable fit
      for(i in 1:chains){init[[i]]$Xna <- array(0, Nna)}

      if(missing(paths)){   ## without inner model
        stanfit <- suppressWarnings(sampling(stanmodels$factorialNA,
                            data = standata, pars = pars,
                            verbose = FALSE, init = init,
                            chains = chains, iter = iter,
                            cores = cores, ...))
      }
      else{  ## with inner model
        handler4()
        pars <- c(pars, "beta")

        standata$Ny = Ny
        standata$idy = array(idy)
        standata$idyi = array(idyi)
        standata$nbeta = array(nbeta)
        standata$idlamb = array(idlamb)

        stanfit <- suppressWarnings(sampling(stanmodels$semNA,
                            data = standata, pars = pars,
                            verbose = FALSE, init = init,
                            chains = chains, iter = iter,
                            cores = cores, ...))
      }
    }
    cat("\nExtracting posterior samples, please wait...\n")
    samples <- list(alpha = rstan::extract(stanfit, permuted = FALSE, pars = "alpha", inc_warmup = FALSE),
                    lambda = rstan::extract(stanfit, permuted = FALSE, pars = "lambda", inc_warmup = FALSE),
                    sigma2 = rstan::extract(stanfit, permuted = FALSE, pars = "sigma2", inc_warmup = FALSE))
    if(stanfit@model_name %in% c("factorialNA", "semNA")){samples$Xna <- rstan::extract(stanfit, permuted = FALSE, pars = "Xna", inc_warmup = FALSE)}
    if(stanfit@model_name %in% c("sem", "semNA")){samples$beta <- rstan::extract(stanfit, permuted = FALSE, pars = "beta", inc_warmup = FALSE)}

    handler5()

    invisible(capture.output(aux <- lapply(samples, function(x)apply(x,3, rstan::monitor, warmup = 0))))
    stats <- matrix(unlist(aux), ncol = 22, byrow = TRUE)
    colnames(stats) <- colnames(aux[[1]][[1]])
    rownames(stats) <- unlist(lapply(aux,names))

    output <- list(stanfit = stanfit,
                   samples = samples,
                   model = stanfit@model_name,
                   mean_loadings = matrix(stats[startsWith(rownames(stats), "alpha"),"mean"], ncol = K),
                   mean_scores =  matrix(stats[startsWith(rownames(stats), "lambda"),"mean"], nrow = K),
                   mean_var = stats[startsWith(rownames(stats), "sigma2"),"mean"],
                   stats = stats
                   )

   if(stanfit@model_name %in% c("factorialNA", "semNA")){output$Xna = stats[startsWith(rownames(stats), "Xna"),"mean"]}
   if(stanfit@model_name %in% c("sem", "semNA")){output$beta = stats[startsWith(rownames(stats), "beta"),"mean"]}

   if(!missing(blocks)){output$blocks <- blocks}
   if(!missing(paths)){output$paths <- paths}

   # HPD intervals
    credint <- list(loadings = t(apply(samples$alpha, 3, function(x)HPDinterval(mcmc(as.vector(x))))),
                    scores = t(apply(samples$lambda, 3, function(x)HPDinterval(mcmc(as.vector(x))))),
                    var = t(apply(samples$sigma2, 3, function(x)HPDinterval(mcmc(as.vector(x)))))
    )
    if(stanfit@model_name %in% c("factorialNA", "semNA")){credint$Xna <-t(apply(samples$Xna, 3, function(x)HPDinterval(mcmc(as.vector(x)))))}
    if(stanfit@model_name %in% c("sem", "semNA")){credint$beta <- t(apply(samples$beta, 3, function(x)HPDinterval(mcmc(as.vector(x)))))}

     # output$credint <- credint
     output$h <- diag(output$mean_loadings %*% t(output$mean_loadings)) ## comunalities
     output$PVTE =  100 * output$h / (output$h + output$mean_var) ## proportion of total variability

     output$SQE <- sum(((output$mean_loadings %*% output$mean_scores))^2)
     output$SQT <- sum((standata$X - mean(standata$X))^2)
     output$AFR2 <- 100 * (1 - output$SQE/output$SQT) # AFR2 = adapted to factor analisys R2

     if(Nna > 0){output$idna <- idna}

     rownames(output$mean_loadings) <- colnames(data);
     colnames(output$mean_loadings) <- names(blocks)
     rownames(output$mean_scores) <- names(blocks);
     colnames(output$mean_scores) <- row_names
     names(output$mean_var) <- colnames(data)
     cat("\nDone!\n")
     class(output) <- "bsem"

    return(output)
  }
