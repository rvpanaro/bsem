#' 'bsem' object print
#'
#' @export
#' @param x an object of class bsem
#' @param digits number of digits to display
#' @param ... further arguments passed to or from print methods
#' @method print bsem
#' @return none
#' @seealso \code{\link[bsem]{bsem::simdata}}, \code{\link[bsem]{bsem::arrayplot}}, \code{\link[bsem]{bsem::summary.sem}}, \code{\link[bsem]{bsem::sem}}, \code{\link[bsem]{bsem::runShiny}}
#'

 print.bsem <-
   function(x, digits = 4, ...){
     savedig <- options(digits = digits)
     on.exit(options(savedig))

     cat("\n\n---\nbsem model: ", x$model, "\n")
     cat("latent variables (outter model): ", length(x$blocks), "\n")
     if(x$model %in%  c("semNA", "sem", "semNAEX", "semEX")){cat("regressions (inner model): ", length(x$paths), "\n")}
     if(x$model %in%  c("factorialNAEX", "factorialEX", "semNAEX", "semEX")){cat("exogenous variables: ", length(x$exogenous), "\n")}

     invisible(capture.output(y <- summary(x, digits = digits)))

    cat("\n\n outter model loadings (alpha):\n")
    for(i in 1:length(x$blocks)){
      cat("\n", names(x$blocks)[i], "\n")
      print(round(y$blocks[[i]],
            digits = digits), digits = digits)
    }


     if(x$model %in% c("semNA", "sem", "semNAEX", "semEX")){
       cat("\n---\n\n\n inner model regression coefficients (beta):\n")
       for(i in 1:length(x$paths)){
         cat("\n", names(x$paths)[i], "\n")
         print(round(y$paths[[i]],
               digits = digits), digits = digits)
       }
     }


    if(x$model %in% c("factorialNAEX", "factorialEX", "semNAEX", "semEX")){
      cat("\n---\n\n\n exogenous variables regression coefficients (gamma) :\n")
      for(i in 1:length(x$exogenous)){
        cat("\n", names(x$exogenous)[i], "\n")
        print(round(y$exogenous[[i]],
                    digits = digits), digits = digits)
      }
    }
    cat("\n---\n")
}

#' 'bsem' object summary
#'
#' @export
#' @param x an object of class spbp
#' @param digits number of digits to display
#' @param ... further arguments passed to or from summary methods
#' @method summary bsem
#' @return none

summary.bsem <-
  function(x,
           digits = 4,
           ...){

    savedig <-
      options(digits = digits)

    on.exit(options(savedig))

    stats <- x$stats

    cat("\n\n---\nbsem model: ", x$model, "\n")
    cat("latent variables (outter model): ", length(x$blocks), "\n")

    if(x$model %in%  c("semNA", "sem", "semNAEX", "semEX")){
      cat("regressions (inner model): ", length(x$paths), "\n")
      }
    if(x$model %in%  c("semNA", "factorialNA", "semNAEX", "factorialNAEX")){
      cat("missing: ", length(x$mean_Xna), "\n")
      }

    cat("\n\n outter model loadings (alpha):\n")

    aux_blocks <- list()
    aux_credint_blocks <- list()

    for(i in 1:length(x$blocks)){
      cat("\n", names(x$blocks)[i], "\n")
      finder <- paste0("alpha[",
                    which(rownames(x$mean_alpha) %in%  x$blocks[[i]]),",",
                    which(rownames(x$mean_lambda) == names(x$blocks)[i]),
                    "]" )

      aux_blocks[[i]] <- matrix(stats[rownames(stats) %in% finder, ],
                                ncol = ncol(stats))
      aux_credint_blocks[[i]] <- matrix(x$credint$alpha[rownames(x$credint$alpha) %in% finder, ],
                                ncol = 2)

      colnames(aux_blocks[[i]]) <- colnames(stats)


      aux_blocks[[i]] <- cbind(
        matrix(aux_blocks[[i]][, c("mean", "50%", "sd")], ncol = 3),
        matrix(aux_credint_blocks[[i]], ncol = 2),
        matrix(aux_blocks[[i]][, c("n_eff", "Rhat")], ncol = 2)
      )
      colnames(aux_blocks[[i]]) <- c("mean", "50%", "sd", "HPD.l", "HPD.u", "n_eff", "Rhat")
      rownames(aux_blocks[[i]]) <- rownames(x$mean_alpha)[rownames(x$mean_alpha) %in%  x$blocks[[i]]]

      print(round(aux_blocks[[i]], digits), digits)
    }

    cat("\n---\n\n idiosyncratic error variances (sigma2):\n")
      aux_credint_sigma2 <- list()
      aux_var <- matrix(stats[startsWith(rownames(stats), "sigma2"), ],
                        ncol = ncol(stats))
      aux_credint_sigma2 <- matrix(x$credint$sigma2,
                            ncol = 2)

      colnames(aux_var) <- colnames(stats)

      aux_var <- cbind(
        matrix(aux_var[, c("mean", "50%", "sd")], ncol = 3),
        matrix(aux_credint_sigma2, ncol = 2),
        matrix(aux_var[, c("n_eff", "Rhat")], ncol = 2)
      )
      colnames(aux_var) <- c("mean", "50%", "sd", "HPD.l", "HPD.u", "n_eff", "Rhat")
      rownames(aux_var) <- paste0("sigma2[", 1:nrow(aux_var), "]")
      print(round(aux_var, digits), digits)

  if(x$model %in% c("semNA", "sem", "semNAEX", "semEX") ){
    cat("\n---\n\n inner model regression coefficients (beta):\n")

    aux_paths <- list()
    aux_credint_paths <- list()

    for(i in 1:length(x$paths)){
      cat("\n", names(x$paths)[i], "\n")

      idx <- c(0,cumsum(lengths(x$paths)))
      finder <- paste0("beta[",
                       (idx[i]+1):idx[i+1],
                       "]")

      aux_paths[[i]] <- matrix(stats[rownames(stats) %in% finder, ],
                               ncol = ncol(stats))
      aux_credint_paths[[i]] <- matrix(x$credint$beta[rownames(x$credint$beta) %in% finder, ],
                            ncol = 2)

      colnames(aux_paths[[i]]) <- colnames(stats)

      aux_paths[[i]] <- cbind(
        matrix(aux_paths[[i]][, c("mean", "50%", "sd")], ncol = 3),
        matrix(aux_credint_paths[[i]], ncol = 2),
        matrix(aux_paths[[i]][, c("n_eff", "Rhat")], ncol = 2)
      )
      colnames(aux_paths[[i]]) <- c("mean", "50%", "sd", "HPD.l", "HPD.u", "n_eff", "Rhat")
      rownames(aux_paths[[i]]) <- x$paths[[i]]
      print(round(aux_paths[[i]], digits), digits)
    }
   }

    if(x$model %in%  c("factorialNA", "semNA", "factorialNAEX", "semNAEX")){
      cat("\n---\n\n  missing observations (Xna): \n")
      aux_Xna <- matrix(stats[startsWith(rownames(stats), "Xna"), ],
                        ncol = ncol(stats))
      colnames(aux_Xna) <- colnames(stats)

      aux_credint_Xna <- matrix(x$credint$Xna,
                           ncol = 2)

      aux <- NULL

      aux <- cbind(
        matrix(aux_Xna[, c("mean", "50%", "sd")], ncol = 3),
        matrix(aux_credint_Xna, ncol = 2),
        matrix(aux_Xna[, c("n_eff", "Rhat")], ncol = 2)
      )

      colnames(aux) <- c("mean", "50%", "sd", "HPD.l", "HPD.u", "n_eff", "Rhat")
      rownames(aux) <- paste0("Xna[", x$idna[,1], ",", x$idna[,2], "]")
      print(round(aux, digits), digits)
    }

    if(x$model %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX") ){
      cat("\n---\n\n exogenous regression coefficients (gamma0, gamma, tau2): \n")

      aux_exogenous <- list()
      aux_credint_exogenous <- list()
      aux <- list()

      for(i in 1:length(x$exogenous)){
        cat("\n", names(x$exogenous)[i], "\n")

        idx <- c(0,cumsum(lengths(x$exogenous)))
        finder <- c(paste0("gamma0[",i,"]"),
                    paste0("gamma[",(idx[i]+1):idx[i+1],"]"),
                    paste0("tau2[",i,"]")
                    )

        aux_exogenous[[i]] <- matrix(stats[rownames(stats) %in% finder, ],
                                 ncol = ncol(stats)
                                 )
        aux_credint_exogenous[[i]] <- rbind(matrix(x$credint$gamma0[rownames(x$credint$gamma0) %in% finder, ],
                                         ncol = 2),
                                  matrix(x$credint$gamma[rownames(x$credint$gamma) %in% finder, ],
                                   ncol = 2),
                                  matrix(x$credint$tau2[rownames(x$credint$tau2) %in% finder, ],
                                         ncol = 2)
        )

        colnames(aux_exogenous[[i]]) <- colnames(stats)

        aux_exogenous[[i]] <- cbind(
          matrix(aux_exogenous[[i]][, c("mean", "50%", "sd")], ncol = 3),
          matrix(aux_credint_exogenous[[i]], ncol = 2),
          matrix(aux_exogenous[[i]][, c("n_eff", "Rhat")], ncol = 2)
        )
        colnames(aux_exogenous[[i]]) <- c("mean", "50%", "sd", "HPD.l", "HPD.u", "n_eff", "Rhat")
        rownames(aux_exogenous[[i]]) <- c("intercept", x$exogenous[[i]], paste0("tau2[", i,"]"))
        print(round(aux_exogenous[[i]], digits), digits)
      }
    }
      cat("\n\n")

    z <- stats[,"Rhat"]

    cat("\nHead Rhat:\n")

    print(head(sort(z, decreasing = T)), digits = digits)

    cat("\nTail Rhat:\n")
    print(tail(sort(z, decreasing = T)), digits = digits)

    cat("\nmedian PTVE: ", median(x$PTVE), "\n")

    if(x$model %in% c("sem", "semNA", "semEX", "semNAEX")){
      cat("R2:", paste0(round(x$AFR2,2), '%'), "\n")
    }
    summ <-list(blocks = aux_blocks,
                var = aux_var,
                stats = stats)

    if(x$model %in% c("sem", "semNA", "semEX", "semNAEX")){
      summ$paths <- aux_paths
    }
    if(x$model %in% c("factorialNA", "semNA", "factorialNAEX", "semNAEX")){
      summ$Xna <- aux_Xna
    }
    if(x$model %in% c("factorialEX", "semEX", "factorialNAEX", "semNAEX")){
      summ$exogenous <- aux_exogenous
    }
    cat("\n---\n")

    return(invisible(summ))
}

