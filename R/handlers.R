## --------------- Priors handling ---------------
handler1 <- function() {
  e <- parent.frame()

  # ----
  ## array of priors for beta
  if(any(!names(e$prior_specs) %in% c("beta", "gamma", "gamma0", "sigma2", "tau2"))){
    stop("prior_specs names should include c('beta', 'gamma', 'gamma0', 'sigma2', 'tau2') only")
  }

  betap <- try(lapply(e$prior_specs$beta, read_prior), silent = T)
  if (is.null(betap[2]) || length(betap) == 0) betap <- lapply("normal(0,1)", read_prior)

  priordist <- sapply(betap, `[[`, 1)
  par1 <- sapply(betap, `[[`, 2)
  par2 <- sapply(betap, `[[`, 3)

  priordist_beta <- sapply(priordist, function(x) {
    switch(x,
      "normal" = 0,
      "cauchy" = 1
    )
  })

  if (any(is.null(unlist(priordist_beta)))) {
    stop(" 'beta prior should be normal or cauchy, try prior_specs = list(beta = normal(...,...), ...) instead'")
  }

  if(as.numeric(par2) < 0){
    stop(priordist, " distribution parameter 2 should be greater than zero.")
  }

  if (exists("paths", envir = e, mode = "list", inherits = F)) {
    # ## Recycling the prior specs
    e$priordist_beta <- array(priordist_beta, dim = sum(lengths(e$paths)))
    e$par1_beta <- array(as.numeric(par1), dim = sum(lengths(e$paths)))
    e$par2_beta <- array(as.numeric(par2), dim = sum(lengths(e$paths)))
  }
  else {
    e$priordist_beta <- array(priordist_beta, dim = 1)
    e$par1_beta <- array(as.numeric(par1), dim = 1)
    e$par2_beta <- array(as.numeric(par2), dim = 1)
  }

  # ----
  ## array of priors for tau2
  sigma2p <- try(lapply(e$prior_specs$sigma2, read_prior), silent = T)
  if (is.na(sigma2p[2]) || length(sigma2p) == 0) sigma2p <- lapply("inv_gamma(2.1, 1.1)", read_prior)

  priordist <- sapply(sigma2p, `[[`, 1)
  par1 <- sapply(sigma2p, `[[`, 2)
  par2 <- sapply(sigma2p, `[[`, 3)

  priordist_sigma2 <- sapply(
    priordist,
    function(x) {
      switch(x,
        "gamma" = 0,
        "inv_gamma" = 1,
        "lognormal" = 2
      )
    }
  )


  if (any(is.null(unlist(priordist_sigma2)))) {
    stop(" sigma2 prior should be gamma, inv_gamma or lognormal, try prior_specs = list(sigma2 = inv_gamma(...,...), ...) instead'")
  }

  if(priordist_sigma2 !=  2 && as.numeric(par1) < 0){
    stop(priordist, " distribution parameter 1 should be greater than zero.")
  }

  if(as.numeric(par2) < 0){
    stop(priordist, " distribution parameter 2 should be greater than zero.")
  }

  ## Recycling the prior specs
  if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
    e$priordist_sigma2 <- array(priordist_sigma2, dim = e$Nv - length(e$exogenous))
    e$par1_sigma2 <- array(as.numeric(par1), dim = e$Nv - length(e$exogenous))
    e$par2_sigma2 <- array(as.numeric(par2), dim = e$Nv - length(e$exogenous))
  }
  else {
    e$priordist_sigma2 <- array(priordist_sigma2, dim = e$Nv)
    e$par1_sigma2 <- array(as.numeric(par1), dim = e$Nv)
    e$par2_sigma2 <- array(as.numeric(par2), dim = e$Nv)
  }

  # ----

  ## array of priors for gamma
  gammap <- try(lapply(e$prior_specs$gamma, read_prior), silent = T)
  if (is.na(gammap[2]) || length(gammap) == 0) gammap <- lapply("normal(0,1)", read_prior)

  priordist <- sapply(gammap, `[[`, 1)
  par1 <- sapply(gammap, `[[`, 2)
  par2 <- sapply(gammap, `[[`, 3)


  priordist_gamma <- sapply(priordist, function(x) {
    switch(x,
      "normal" = 0,
      "cauchy" = 1
    )
  })

  if (any(is.null(unlist(priordist_gamma)))) {
    stop(" gamma prior should be normal or cauchy, try prior_specs = list(gamma = normal(...,...), ...) instead'")
  }

  if(as.numeric(par2) < 0){
    stop(priordist, " distribution parameter 2 should be greater than zero.")
  }

  # ## Recycling the prior specs
  if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
    e$priordist_gamma <- array(priordist_gamma, dim = sum(lengths(e$exogenous)))
    e$par1_gamma <- array(as.numeric(par1), dim = sum(lengths(e$exogenous)))
    e$par2_gamma <- array(as.numeric(par2), dim = sum(lengths(e$exogenous)))
  }
  else {
    e$priordist_gamma <- array(priordist_gamma, dim = 1)
    e$par1_gamma <- array(as.numeric(par1), dim = 1)
    e$par2_gamma <- array(as.numeric(par2), dim = 1)
  }
  ## array of priors for gamma
  gamma0p <- try(lapply(e$prior_specs$gamma0, read_prior), silent = T)

  if (is.na(gamma0p[2]) || length(gamma0p) == 0) gamma0p <- lapply("normal(0,1)", read_prior)

  priordist <- sapply(gamma0p, `[[`, 1)
  par1 <- sapply(gamma0p, `[[`, 2)
  par2 <- sapply(gamma0p, `[[`, 3)

  priordist_gamma0 <- sapply(priordist, function(x) {
    switch(x,
      "normal" = 0,
      "cauchy" = 1
    )
  })

  if (any(is.null(unlist(priordist_gamma0)))) {
    stop(" gamma0 prior should be normal or cauchy, try prior_specs = list(gamma0 = normal(...,...), ...) instead'")
  }

  if(as.numeric(par2) < 0){
    stop(priordist, " distribution parameter 2 should be greater than zero.")
  }

  # ## Recycling the prior specs
  if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
    e$priordist_gamma0 <- array(priordist_gamma0, dim = length(e$exogenous))
    e$par1_gamma0 <- array(as.numeric(par1), dim = length(e$exogenous))
    e$par2_gamma0 <- array(as.numeric(par2), dim = length(e$exogenous))
  }
  else {
    e$priordist_gamma0 <- array(priordist_gamma0, dim = 1)
    e$par1_gamma0 <- array(as.numeric(par1), dim = 1)
    e$par2_gamma0 <- array(as.numeric(par2), dim = 1)
  }

  # ----
  ## array of priors for tau2
  tau2p <- try(lapply(e$prior_specs$tau2, read_prior), silent = T)
  if (is.na(tau2p[2]) || length(tau2p) == 0) tau2p <- lapply("inv_gamma(2.1,1.1)", read_prior)

  priordist <- sapply(tau2p, `[[`, 1)
  par1 <- sapply(tau2p, `[[`, 2)
  par2 <- sapply(tau2p, `[[`, 3)

  priordist_tau2 <- sapply(
    priordist,
    function(x) {
      switch(x,
        "gamma" = 0,
        "inv_gamma" = 1,
        "lognormal" = 2
      )
    }
  )

  if (any(is.null(unlist(priordist_tau2)))) {
    stop(" tau2 prior should be gamma, inv_gamma or lognormal, try prior_specs = list(tau2 = inv_gamma(...,...), ...) instead'")
  }

  if(as.numeric(par2) < 0){
    stop(priordist, " distribution parameter 2 should be greater than zero.")
  }

  if(priordist_tau2 !=  2 && as.numeric(par1) < 0){
    stop(priordist, " distribution parameter 1 should be greater than zero.")
  }

  if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
    ## Recycling the prior specs
    e$priordist_tau2 <- array(priordist_tau2, dim = length(e$exogenous))
    e$par1_tau2 <- array(as.numeric(par1), dim = length(e$exogenous))
    e$par2_tau2 <- array(as.numeric(par2), dim = length(e$exogenous))
  }
  else {
    e$priordist_tau2 <- array(priordist_tau2, dim = length(e$exogenous))
    e$par1_tau2 <- array(as.numeric(par1), dim = length(e$exogenous))
    e$par2_tau2 <- array(as.numeric(par2), dim = length(e$exogenous))
  }
}

## --------------- Extra args error handling ---------------
##  ... arguments directly passed to `rstan::stan`, handles typos
handler2 <- function() {
  e <- parent.frame()

  if (length(e$stanArgs)) {
    stanformals <- names(c(formals(rstan::stan))) # legal arg names
    aux <- pmatch(names(e$stanArgs), stanformals, nomatch = 0)

    if (any(aux == 0)) {
      stop(gettextf(" arguments %s not matched", paste(names(e$stanArgs)[aux == 0], collapse = ", ")))
    }
  }
}

## --------------- Initial values handling ---------------
##  ... arguments directly passed to `rstan::stan`, handles typos

handler3 <- function(missing_signals) {
  e <- parent.frame()

  # Chutes iniciais para as cadeias do MCMC.
  if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
    aux <- array(0, c(e$Nv - length(e$exogenous), e$K))
  }
  else {
    aux <- array(0, c(e$Nv, e$K))
  }

  if (missing_signals) {
    for (k in 1:e$K) {
      aux[e$B[[k]], k] <- runif(lengths(e$B)[k], 0.5, 2) * sign(rnorm(lengths(e$B)[k]))
    }
    aux[-unlist(e$B), ] <- runif(e$K, 0.5, 2) * sign(rnorm(e$K))
  }
  else {
    if (length(e$signals) != length(e$B)) {
      stop("signals misspecification: length(signals) and length(blocks) not matched")
    }
    if (any(lengths(e$signals) != lengths(e$B))) {
      stop("signals misspecification: lenghts(signals) and lengths(blocks) not matched")
    }

    for (k in 1:e$K) {
      aux[e$B[[k]], k] <- 2 * e$signals[[k]]
    }
    aux[-unlist(e$B), ] <- runif(e$K, 0.5, 2) * sign(rnorm(e$K))
  }

  init <- list()
  for (i in 1:e$chains) {
    init[[i]] <- list(
      alpha = aux,
      lambda = array(runif(e$K * e$Ne, -0.1, 0.1), c(e$K, e$Ne))
    )
    if (exists("exogenous", envir = e, mode = "list", inherits = F)) {
      init[[i]]$sigma2 <- runif(e$Nv - length(e$exogenous))
    }
  }
  init$lambda_tilde <- init$lambda
  e$init <- init
}

## --------------- path modeling handler ---------------
handler4 <- function() {
  e <- parent.frame()
  e$pars <- c(e$pars, "beta")

  for (i in 1:length(e$paths)) {
    if (class(e$paths[[i]]) == "character") {
      aux <- as.numeric(unlist(e$paths) %in% names(e$blocks))

      if (any(aux == 0)) {
        stop(gettextf(
          "argument misspecification: argument %s not matched",
          unlist(e$paths)[aux == 0]
        ))
      }
    }
    else if (class(e$paths[[i]]) %in% c("numeric", "integer", "vector")) {
      if (!all(e$paths[[i]] == floor(e$paths[[i]]))) {
        stop(gettextf(
          "path[s] %s positioning must be integer",
          paste0(i, collapse = ", ")
        ))
      }
      aux <- !e$paths[[i]] %in% 1:length(e$blocks)
      if (any(aux)) {
        stop(gettextf(
          "paths misspecification: %s exceeds length(blocks)",
          paste0(e$paths[[i]][aux == T], collapse = ", ")
        ))
      }
      e$paths[[i]] <-names(e$blocks)[e$paths[[i]]] # search by variable names
    }
    else {
      stop("paths misspecification: paths must be a list of character or integer")
    }
  }

  aux <- unlist(e$paths) %in% names(e$blocks)

  if (any(aux == 0)) {
    stop(gettextf(
      " latent variable[s] %s in unlist(paths) not matched names(blocks)",
      paste(unlist(e$paths)[aux == F], collapse = ", ")
    ))
  }

  aux <- names(e$paths) %in% names(e$blocks)

  if (any(aux == 0)) {
    stop(gettextf(
      " latent variable[s] %s in names(paths) not matched names(blocks)",
      paste(names(e$paths)[aux == F], collapse = ", ")
    ))
  }

  e$standata$idy <- which(names(e$blocks) %in% names(e$paths))
  e$standata$idyi <- which(!(names(e$blocks) %in% names(e$paths)))
  e$standata$Ny <- length(e$paths)
  e$standata$nbeta <- lengths(e$paths)

  e$standata$idy <- array(e$standata$idy)
  e$standata$idyi <- array(e$standata$idyi)
  e$standata$nbeta <- array(e$standata$nbeta)


  e$standata$idlamb <- unlist(sapply(1:e$standata$Ny, function(x) {
    which(names(e$blocks) %in% e$paths[[x]], arr.ind = T)
  }))
  e$standata$idlamb <- array(e$standata$idlamb)
}

## --------------- exogenous modeling handler ---------------
handler5 <- function() {
  e <- parent.frame()

  e$pars <- c(e$pars, "gamma", "tau2", "gamma0")

  if(is.null(names(e$exogenous))){
    if(any(is.na(e$data[,1:length(e$exogenous)])))
      stop("missing variables in exogenous variable not allowed")
  }
  else{
    if(any(is.na(e$data[, names(e$exogenous)])))
      stop("missing variables in exogenous variable not allowed")
  }

  aux <- unlist(e$exogenous) %in% unlist(e$blocks)

  if (any(aux != 0)) {
    stop(gettextf(
      "exogenous misspecification: variable[s] %s in unlist(exogenous) matched unlist(blocks)",
      paste(unlist(e$exogenous)[aux == T], collapse = ", ")
    ))
  }

  for (i in 1:length(e$exogenous)) {
    if (class(e$exogenous[[i]]) == "character") {
      aux <- as.numeric(e$exogenous[[i]] %in% names(e$blocks))

      if (any(aux == 0)) {
        stop(gettextf(
          "exogenous misspecification: colnames %s not matched",
          paste(unlist(exogenous)[aux == 0], collapse = ", ")
        ))
      }
      e$standata$exogenous[[i]] <- which(names(e$blocks) %in% e$exogenous[[i]]) # search by variable names
    }
    else if (class(e$exogenous[[i]]) %in% c("numeric", "integer", "vector")) {
      if (!all(e$exogenous[[i]] == floor(e$exogenous[[i]]))) {
        stop(gettextf(
          "exogenous misspecification: exogenous %s must be integer",
          paste0(i, collapse = ", ")
        ))
      }

      aux <- !e$exogenous[[i]] %in% 1:ncol(e$data)
      if (any(aux)) {
        stop(gettextf(
          "exogenous misspecification: there is no columns %s in data.frame",
          paste0(e$exogenous[[i]][aux == T], collapse = ", ")
        ))
      }
      e$exogenous[[i]] <- names(e$blocks)[e$exogenous[[i]]]
    }
    else {
      stop("exogenous misspecification: exogenous must be either character, integer, numeric or vector")
    }
  }

  e$standata$idex <- which(colnames(e$data) %in% names(e$exogenous))
  e$standata$idexi <- which(unlist(e$exogenous) %in% names(e$blocks))
  e$standata$Nex <- length(e$exogenous)
  e$standata$ngamma <- lengths(e$exogenous)

  e$standata$idlambex <- unlist(sapply(1:e$standata$Nex, function(x) {
    which(names(e$blocks) %in% e$exogenous[[x]], arr.ind = T)
  }))

  e$standata$idex <- array(e$standata$idex)
  e$standata$idexi <- array(e$standata$idexi)
  e$standata$idlambex <- array(e$standata$idlambex)
  e$standata$ngamma <- array(e$standata$ngamma)

  e$standata$idob <- e$standata$idob[which(!e$standata$idob[, 1] %in% e$standata$idex), ]
  e$standata$asc <- as.numeric(factor(e$standata$idob[, 1]))
  e$standata$Nob <- nrow(e$standata$idob)

  if(e$Nna>0){
    e$standata$idna <- matrix(e$standata$idna[which(!e$standata$idna[, 1] %in% e$standata$idex), ], ncol = 2)
    e$standata$asc2 <- array(as.numeric(factor(e$standata$idna[, 1])))
    e$standata$Nna <- nrow(e$standata$idna)
  }
}

handler6 <- function() {
  e <- parent.frame()
  for (k in 1:e$K) {

    id_alp <- paste0("alpha[", e$B[[k]], ",", k, "]")

    c_means <- apply(array(e$samples$alpha[, , id_alp], c(dim(e$samples$alpha)[1], dim(e$samples$alpha)[2], length(id_alp))), c(2, 3), mean)
    r_signal <- apply(c_means, 2, checksignals)
    aux <- 2 * (c_means > 0) - 1

    if (all(!r_signal)) {
      asum <- abs(apply(aux, 1, sum))
      if (all(asum == asum[1])) {
        for (i in which(endsWith(x = dimnames(e$samples$alpha)[3]$parameters, suffix = paste0(k, "]")))) {
          e$samples$alpha[, , i] <- e$samples$alpha[, , i] %*% -diag(2 * (c_means[, 1] > 0) - 1)
        }
        for (i in which(startsWith(x = dimnames(e$samples$lambda)[3]$parameters, prefix = paste0("lambda[", k)))) {
          e$samples$lambda[, , i] <- e$samples$lambda[, , i] %*% -diag(2 * (c_means[, 1] > 0) - 1)
        }
        if (e$stanfit@model_name %in% c("sem", "semNA", "semEX", "semNAEX")) {
          if (k %in% array(e$standata$idlamb)) {
            for (j in which(array(e$standata$idlamb) == k)) {
              e$samples$beta[, , j] <- e$samples$beta[, , j] %*% -diag(2 * (c_means[, 1] > 0) - 1)
            }
          }
          if (k %in% 1:e$standata$Ny) {
            idx <- cbind(c(1, cumsum(e$standata$nbeta[-length(e$standata$nbeta)]) + 1), c(cumsum(e$standata$nbeta)))
            for (j in seq(idx[k, 1], idx[k, 2])) {
              e$samples$beta[, , j] <- e$samples$beta[, , j] %*% -diag(2 * (c_means[, 1] > 0) - 1)
            }
          }
        }
        if (e$stanfit@model_name %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX")) {
          if (k %in% array(e$standata$idex)) {
            for (j in which(array(e$standata$idex) == k)) {
              e$samples$gamma[, , j] <- e$samples$gamma[, , j] %*% -diag(2 * (c_means[, 1] > 0) - 1)
            }
          }
        }
      }
    }
  }
}
