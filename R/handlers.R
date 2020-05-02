## --------------- Priors handling ---------------
handler1 <- function(){
  e <- parent.frame()

  # ----
  ## array of priors for the idiosyncratic noise
  noisep <- try(lapply(e$priors$noise, read_prior), silent = T)
  priordist <- sapply(noisep, `[[`, 1)
  par1 <- sapply(noisep, `[[`,2)
  par2 <- sapply(noisep, `[[`,3)


  priordist_noise <- sapply(priordist,
                            function(x){switch(x,
                                     "normal" = 0, "gamma" = 1,
                                     "inv_gamma" = 2, "lognormal" = 3)}
                            )
  ## Recycling the prior specs
  e$priordist_noise <- array(priordist_noise, dim = e$Nv)
  e$par1_noise <- array(as.numeric(par1), dim = e$Nv)
  e$par2_noise <- array(as.numeric(par2), dim = e$Nv)

  # ----
  ## array of priors for the regression coefficients

  # betap <- try(lapply(e$priors$beta, read_prior), silent = T)
  # if(is.na(betap[2])) betap <- c("normal", "0", "1")
  #
  # priordist <- sapply(betap, `[[`, 1)
  # par1 <- sapply(betap, `[[`,2)
  # par2 <- sapply(betap, `[[`,3)
  #
  # priordist_beta <- sapply(priordist, function(x){switch(x,
  #                                             "normal" = 0,
  #                                             "cauchy" = 1)}
  #                          )
  # ## Recycling the prior specs
  # e$priordist_beta <- array(priordist_beta, dim = q)
  # e$par1_beta <- array(as.numeric(par1), dim = q)
  # e$par2_beta <- array(as.numeric(par2), dim = q)
}

## --------------- Extra args error handling ---------------
##  ... arguments directly passed to `rstan::stan`, handles typos
handler2 <- function(){
  e <- parent.frame()

  if (length(e$stanArgs)) {
    stanformals <- names(c(formals(rstan::stan))) #legal arg names
    aux <- pmatch(names(e$stanArgs), stanformals, nomatch = 0)

    if (any(aux == 0))
      stop(gettextf("Argument %s not matched", names(e$stanArgs)[aux==0]))
  }
}

read_prior <- function(prior){
  aux <- unlist(strsplit(prior, "\\("))
  dist <- aux[1]
  aux2 <- unlist(strsplit(aux[2], "\\)"))[1]
  val <- unlist(strsplit(aux2, "\\,"))
  return(c(dist, val))
}

## --------------- Initial values handling ---------------
##  ... arguments directly passed to `rstan::stan`, handles typos

handler3 <- function(missing_signals){
  e <- parent.frame()

  # Chutes iniciais para as cadeias do MCMC.
  aux <- array(0, c(e$Nv, e$K))

  if(missing_signals){
    for(k in 1:e$K){
        aux[e$B[[k]],k] <- runif(lengths(e$B)[k], -0.1, 0.1)
      }
      aux[-unlist(e$B),] <- runif(e$K, -0.1, 0.1)
    }
  else{
    if(length(e$signals) != length(e$B) || lengths(e$signals) != lengths(e$B)){
      stop("signals and blocks length or lengths do not match")
    }

    for(k in 1:e$K){
        aux[e$B[[k]], k] <- e$signals[[k]]
      }
      aux[-unlist(e$B),] <- e$signals[[k]]
    }

  init <-  list()
  for(i in 1:e$chains){
    init[[i]] = list(alpha = aux,
                     lambda = array(runif(e$K*e$Ne, -0.1,0.1), c(e$K, e$Ne)),
                     sigma2 = runif(e$Nv))
  }
  init$lambda_tilde = init$lambda
  e$init <- init
}

## --------------- path modeling handler ---------------
handler4 <- function(){
  e <- parent.frame()

  aux <- unlist(e$paths)  %in%  names(e$blocks)

  if(any(aux == 0)){
    stop(gettextf("Argument ", unlist(e$paths)[aux==F], " not matched"))
  }

  e$idy <- which(names(e$blocks) %in% names(e$paths))
  e$idyi <- which(!(names(e$blocks) %in% names(e$paths)))

  e$Ny <- length(e$paths)
  e$nbeta <- lengths(e$paths)

  e$idlamb <- unlist(sapply(1:e$Ny, function(x)
    which(names(e$blocks) %in% e$paths[[x]], arr.ind = T)
  )
  )
}

handler5 <- function(){
  e <- parent.frame()
  for(k in 1:e$K){
    id_alp <- paste0("alpha[", e$B[[k]],",", k,"]")

    c_means <- apply(array(e$samples$alpha[,,id_alp],c(dim(e$samples$alpha)[1],dim(e$samples$alpha)[2],length(id_alp))), c(2,3), mean)
    r_signal <- apply(c_means, 2, checksignals)
    aux <- 2*(c_means>0) -1

    if(all(!r_signal)){
      asum <- abs(apply(aux, 1, sum))
      if(all(asum == asum[1])){
        for(i in which(endsWith(x = dimnames(e$samples$alpha)[3]$parameters, suffix = paste0(k,"]")))){
          e$samples$alpha[,,i]<- e$samples$alpha[,,i] %*% -diag(2*(c_means[,1]>0) -1)
        }
        for(i in which(startsWith(x = dimnames(e$samples$lambda)[3]$parameters, prefix = paste0("lambda[",k)))){
          e$samples$lambda[,,i]<- e$samples$lambda[,,i] %*% -diag(2*(c_means[,1]>0) -1)
        }
        print(c_means)
          if(e$stanfit@model_name %in% c("sem", "semNA")){
            if(k %in% array(e$idlamb)){
              print(paste("idlamb", which(array(e$idlamb) == k)))
              for(j in which(array(e$idlamb) == k)){
                e$samples$beta[,,j]<- e$samples$beta[,,j] %*% -diag(2*(c_means[,1]>0) -1)
              }
              print(paste("idy", e$idlamb[,which(e$idy == k)]))
              # for(j in e$idlamb[,which(e$idy == k)]){
              #   e$samples$beta[,,j]<- e$samples$beta[,,j] %*% -diag(2*(c_means[,1]>0) -1)
              # }
            }
          }
        }
      }
    }
  }




