#' 'bsem' object print
#'
#' @export
#' @param x an object of class spbp
#' @param digits number of digits to display
#' @param ... further arguments passed to or from print methods
#' @method print bsem
#' @return none
#'

 print.bsem <-
   function(x, digits = max(getOption('digits')-4, 3), ...){
     savedig <- options(digits = digits)
     on.exit(options(savedig))

     cat("\n\n---\nbsem model: ", x$model, "\n")
     cat("latent variables (outter model): ", length(x$blocks), "\n")
     if(x$model %in%  c("semNA", "sem")){cat("regressions (inner model): ", length(x$paths), "\n")}
     # if(x$model %in%  c("semNA", "factorialNA")){cat("missing: ", length(x$mean_Xna), "\n")}

     invisible(capture.output(y <- summary(x, digits = digits)))

    cat("\n\n outer model loadings:\n")

    for(i in 1:length(x$blocks)){
      cat("\n", names(x$blocks)[i], "\n")
      print(y$blocks[[i]][, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")],
            digits = digits)
    }

     if(x$model %in% c("semNA", "sem")){
       cat("\n---\n\n\n inner model regression coefficients:\n")
       for(i in 1:length(x$paths)){
         cat("\n", names(x$paths)[i], "\n")
         print(y$paths[[i]][, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")],
               digits = digits)
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
  function(x, digits = max(getOption('digits')-4, 3), ...){
    savedig <- options(digits = digits)
    on.exit(options(savedig))

    stats <- x$stats

    cat("\n\n---\nbsem model: ", x$model, "\n")
    cat("latent variables (outter model): ", length(x$blocks), "\n")
    if(x$model %in%  c("semNA", "sem")){cat("regressions (inner model): ", length(x$paths), "\n")}
    if(x$model %in%  c("semNA", "factorialNA")){cat("missing: ", length(x$mean_Xna), "\n")}

    cat("\n\n outer model loadings:\n")

    aux_blocks <- list()

    for(i in 1:length(x$blocks)){
      cat("\n", names(x$blocks)[i], "\n")
      finder <- paste0("alpha[",
                    which(rownames(x$mean_loadings) %in%  x$blocks[[i]]),",",
                    which(rownames(x$mean_scores) == names(x$blocks)[i]),
                    "]" )

      aux_blocks[[i]] <- matrix(stats[rownames(stats) %in% finder, ], ncol = ncol(stats))
      colnames(aux_blocks[[i]]) <- colnames(stats)
      rownames(aux_blocks[[i]]) <- rownames(x$mean_loadings)[rownames(x$mean_loadings) %in%  x$blocks[[i]]]

      print(round(aux_blocks[[i]][, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")], digits))

      }

    cat("\n---\n\n variances:\n")

      aux_var <- matrix(stats[startsWith(rownames(stats), "sigma2"), ], ncol = ncol(stats))
      colnames(aux_var) <- colnames(stats)
      rownames(aux_var) <- paste0("var[", 1:nrow(aux_var), "]")

      print(round(aux_var[, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")], digits))

  if(x$model %in% c("semNA", "sem") ){
    cat("\n---\n\n inner model regression coefficients:\n")

    aux_paths <- list()

    for(i in 1:length(x$paths)){
      cat("\n", names(x$paths)[i], "\n")

      idx <- c(0,cumsum(lengths(x$paths)))
      finder <- paste0("beta[",
                       (idx[i]+1):idx[i+1],
                       "]")

      aux_paths[[i]] <- matrix(stats[rownames(stats) %in% finder, ], ncol = ncol(stats))
      colnames(aux_paths[[i]]) <- colnames(stats)
      rownames(aux_paths[[i]]) <- x$paths[[i]]

      print(round(aux_paths[[i]][, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")], digits))
    }
    }

    if(x$model %in%  c("factorialNA", "semNA")){
      cat("\n---\n\n  missing observations: \n")
      aux_Xna <- matrix(stats[startsWith(rownames(stats), "Xna"), ], ncol = ncol(stats))
      colnames(aux_Xna) <- colnames(stats)
      rownames(aux_Xna) <- paste0("Xna[", x$idna[,1], ",", x$idna[,2], "]")

      print(round(aux_Xna[, c("mean", "se_mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat")], digits))
    }
    cat("\n\n")

    z <- stats[,"Rhat"]

    cat("\nHead Rhat:\n")

    print(head(sort(z, decreasing = T)), digits = digits)

    cat("\nTail Rhat:\n")
    print(tail(sort(z, decreasing = T)), digits = digits)

    cat("\nmedian PVTE: ", median(x$PVTE), "\n")

    if(x$model %in% c("sem", "semNA")){
      cat("SQT:", median(x$SQE), "SQE:", median(x$SQT), "AFR2:", paste0(round(x$AFR2,2), '%'), "\n")
    }
    summ <-list(blocks = aux_blocks,
                var = aux_var,
                stats = stats)

    if(x$model %in% c("sem", "semNA")){
      summ$paths <- aux_paths
    }
    if(x$model %in% c("factorialNA", "semNA")){
      summ$Xna <- aux_Xna
    }
    cat("\n---\n")

    return(invisible(summ))
}

