#' 'bsem' object plot
#'
#' @export
#' @param x an object of class bsem
#' @param digits number of digits to display
#' @param fontsize edge fontsize (estimates character size)
#' @param width lines width
#' @param size  nodes size (for both: ellipses and boxes)
#' @param ... further arguments passed to \code{visNetwork::visNetwork} function
#' @method plot bsem
#' @return a visNetwork graph:
#' \describe{
#'    \item{}{The ellipsoidal nodes represent the latent variables}
#'    \item{}{The boxes represent the manifest variables}
#'    \item{}{The dashed lines represent the linear relations between latent scores or between latent scores and exogenous variables}
#'    \item{}{The solid lines represent the relationship between the manifest an latent variables}
#'    \item{}{The recursive solid lines refers to the error variance estimate of each manifest or exogenous variable}
#' }
#' @importFrom DiagrammeR create_node_df create_edge_df
#' @importFrom viridis viridis
#' @importFrom visNetwork visNetwork visNodes visOptions
#' @importFrom magrittr %>%
#' @author RVPanaro
#' @seealso \code{\link[bsem]{sem}}, \code{\link[bsem]{simdata}}, \code{\link[bsem]{arrayplot}}, \code{\link[bsem]{summary.bsem}}, \code{\link[bsem]{print.bsem}}
#' @examples
#'
#' dt <- bsem::simdata()
#' names(dt)
#' \donttest{
#'
#' semfit <- bsem::sem(
#'   data = dt$data,
#'   blocks = dt$blocks,
#'   paths = dt$paths,
#'   exogenous = dt$exogenous,
#'   signals = dt$signals,
#'   cores = 1
#' )
#' plot(semfit)
#' }
#'
plot.bsem <-
  function(x,
           digits = 2,
           fontsize = 15,
           width = 5,
           size = 10,
           ...) {
    invisible(capture.output(y <- summary(x, digits = digits)))

    # x <- fit
    blocks <- y[["blocks"]]
    var <- y[["var"]]

    if (x$model %in% c("factorialEX", "factorialNAEX", "semEX", "semNAEX")) {
      lvl <- unique(c(unlist(x$blocks), names(x$blocks), names(x$exogenous)))
    }
    else {
      lvl <- unique(c(unlist(x$blocks), names(x$blocks)))
    }


    if (x$model %in% c("semEX", "semNAEX")) {
      paths <- y[["paths"]]
      exogenous <- y[["exogenous"]]

      from <- na.omit(factor(c(
        unlist(x$blocks), unlist(x$paths), unlist(x$exogenous),
        names(x$mean_sigma2), names(x$mean_tau2)
      ),
      levels = lvl
      ))
      to <- na.omit(factor(c(
        rep(names(x$blocks), lengths(x$blocks)),
        rep(names(x$paths), lengths(x$paths)),
        rep(names(x$exogenous), lengths(x$exogenous)),
        names(x$mean_sigma2), names(x$mean_tau2)
      ),
      levels = lvl
      ))

      lbl <- c(
        format(round(unlist(sapply(1:length(blocks), function(x) blocks[[x]][, "mean"])), digits), digits = digits),
        format(round(unlist(sapply(1:length(paths), function(x) paths[[x]][, "mean"])), digits), digits = digits),
        format(round(unlist(sapply(1:length(exogenous), function(y) exogenous[[y]][-c(1, lengths(x$exogenous)[y] + 2), "mean"])), digits), digits = digits),
        format(round(x$mean_sigma2[names(x$mean_sigma2) %in% lvl], digits), digits = digits),
        format(round(x$mean_tau2[names(x$mean_tau2) %in% lvl], digits), digits = digits)
      )

      dashes <- c(
        rep(c(F, T), c(sum(lengths(x$blocks)), sum(length(x$blocks)))),
        rep(c(T), c(sum(length(x$blocks)))),
        !names(x$mean_sigma2) %in% lvl,
        !names(x$mean_tau2) %in% lvl
      )
    }
    else if (x$model %in% c("sem", "semNA")) {
      paths <- y[["paths"]]

      from <- na.omit(factor(c(
        unlist(x$blocks), unlist(x$paths),
        names(x$mean_sigma2)
      ),
      levels = lvl
      ))
      to <- na.omit(factor(c(
        rep(names(x$blocks), lengths(x$blocks)),
        rep(names(x$paths), lengths(x$paths)),
        names(x$mean_sigma2)
      ),
      levels = lvl
      ))
      lbl <- c(
        format(round(unlist(sapply(1:length(blocks), function(x) blocks[[x]][, "mean"])), digits), digits = digits),
        format(round(unlist(sapply(1:length(paths), function(x) paths[[x]][, "mean"])), digits), digits = digits),
        format(round(x$mean_sigma2[names(x$mean_sigma2) %in% lvl], digits), digits = digits)
      )

      dashes <- c(
        rep(c(F, T), c(sum(lengths(x$blocks)), sum(length(x$blocks)))),
        !names(x$mean_sigma2) %in% lvl
      )
    }
    else if (x$model %in% c("factorialEX", "factorialNAEX")) {
      exogenous <- y[["exogenous"]]

      from <- na.omit(factor(c(
        unlist(x$blocks), unlist(x$exogenous),
        names(x$mean_sigma2), names(x$mean_tau2)
      ),
      levels = lvl
      ))
      to <- na.omit(factor(c(
        rep(names(x$blocks), lengths(x$blocks)),
        rep(names(x$exogenous), lengths(x$exogenous)),
        names(x$mean_sigma2), names(x$mean_tau2)
      ),
      levels = lvl
      ))

      lbl <- c(
        format(round(unlist(sapply(1:length(blocks), function(x) blocks[[x]][, "mean"])), digits), digits = digits),
        format(round(unlist(sapply(1:length(exogenous), function(y) exogenous[[y]][-c(1, lengths(x$exogenous)[y] + 2), "mean"])), digits), digits = digits),
        format(round(x$mean_sigma2[names(x$mean_sigma2) %in% lvl], digits), digits = digits),
        format(round(x$mean_tau2[names(x$mean_tau2) %in% lvl], digits), digits = digits)
      )

      dashes <- c(
        rep(c(F, T), c(sum(lengths(x$blocks)), sum(length(x$blocks)))),
        !names(x$mean_sigma2) %in% lvl,
        !names(x$mean_tau2) %in% lvl
      )
    }
    else {
      from <- na.omit(factor(c(
        unlist(x$blocks),
        names(x$mean_sigma2)
      ),
      levels = lvl
      ))
      to <- na.omit(factor(c(
        rep(names(x$blocks), lengths(x$blocks)),
        names(x$mean_sigma2)
      ),
      levels = lvl
      ))
      lbl <- c(
        format(round(unlist(sapply(1:length(blocks), function(x) blocks[[x]][, "mean"])), digits), digits = digits),
        format(round(x$mean_sigma2[names(x$mean_sigma2) %in% lvl], digits), digits = digits)
      )
      dashes <- F
    }

    edge <- DiagrammeR::create_edge_df(
      from = from,
      to = to,
      label = as.character(lbl),
      smooth = T,
      arrows = "to",
      dashes = dashes,
      width = min(abs(as.numeric(lbl)) * width, 5),
      font.size = fontsize
    )

    if (x$model %in% c("factorial", "factorialNA", "sem", "semNA")) {
      nn <- c(length(unique(unlist(x$blocks))), length(x$blocks))
      col <- factor(c(rep(names(x$blocks), lengths(x$blocks)), names(x$blocks)))
      shp <- rep(c("box", "ellipse"), nn)
    }
    else {
      nn <- c(length(unique(unlist(x$blocks))), length(x$blocks), length(x$exogenous))
      col <- factor(c(rep(names(x$blocks), lengths(x$blocks)), names(x$blocks), names(x$exogenous)))
      shp <- rep(c("box", "ellipse", "circle"), nn)
    }

    node <- DiagrammeR::create_node_df(
      n = sum(nn),
      nodes = lvl,
      label = lvl,
      shape = shp,
      color = viridis(length(levels(col)) + 1)[col[1:length(lvl)]],
      group = col[1:length(lvl)],
      style = "filled",
      fontname = "Roboto",
      fontcolor = "white",
      shadow = T
    )

    # We can plot the graph directly
    visNetwork::visNetwork(nodes = node, edges = edge, ...) %>%
      visNetwork::visOptions(highlightNearest = TRUE) %>%
      visNetwork::visNodes(font = list(color = "white"), size = size)
  }
