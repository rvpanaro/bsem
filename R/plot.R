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
#'    \item{}{The dashed lines represent the linear relations between latent scores}
#'    \item{}{The solid lines represent the relationship between the manifest an latent variables}
#'    \item{}{The recursive solid lines refers to the error variance estimate of each manifest variable}
#' }
#' @importFrom DiagrammeR create_node_df create_edge_df
#' @importFrom viridis viridis
#' @importFrom visNetwork visNetwork visNodes visOptions
#' @importFrom magrittr %>%
#'

plot.bsem <-
  function(x, digits = 2, fontsize = 15, width = 2, size = 10, ...){
    invisible(capture.output(y <- summary(x, digits = digits)))

    blocks <- y[["blocks"]]
    var <- y[["var"]]
    lvl <- unique(c(unlist(x$blocks), names(x$blocks)))

    if(x$model %in% c("sem", "semNA")){
      paths  <- y[["paths"]]

      from <- na.omit(factor(c(unlist(x$blocks), unlist(x$paths),
                       names(x$mean_var)),
                     levels = lvl))
      to   <- na.omit(factor(c(rep(names(x$blocks), lengths(x$blocks)),
                       rep(names(x$paths), lengths(x$paths)),
                       names(x$mean_var)),
                     levels = lvl))
      lbl <- c(format(round(unlist(sapply(1:length(blocks), function(x)blocks[[x]][,"mean"])), digits),digits = digits),
               format(round(unlist(sapply(1:length(paths), function(x)paths[[x]][,"mean"])), digits),digits = digits),
               format(round(x$mean_var[names(x$mean_var) %in% lvl], digits), digits = digits))

      dashes <- c(rep(c(F, T), c(sum(lengths(x$blocks)), sum(length(x$blocks)))),  !names(x$mean_var) %in% lvl)
    }
    else{

      from <- na.omit(factor(c(unlist(x$blocks),
                       names(x$mean_var)),
                     levels = lvl))
      to   <- na.omit(factor(c(rep(names(x$blocks), lengths(x$blocks)),
                       names(x$mean_var)),
                     levels = lvl))
      lbl <- c(format(round(unlist(sapply(1:length(blocks), function(x)blocks[[x]][,"mean"])), digits), digits = digits),
               format(round(x$mean_var[names(x$mean_var) %in% lvl], digits), digits = digits))
      dashes <- F
    }

    edge <- DiagrammeR::create_edge_df(from = from,
                     to = to,
                     label = as.character(lbl),
                     smooth= T,
                     arrows = "to",
                     dashes = dashes,
                     width = abs(as.numeric(lbl)) * width,
                     font.size = fontsize
                     # ,dashes = dashes
                     )

  nn <-  c(length(unique(unlist(x$blocks))), sum(length(x$blocks)))
  col <- factor(c(rep(names(x$blocks), lengths(x$blocks)), names(x$blocks)))


  node <- DiagrammeR::create_node_df(n = sum(nn),
                       nodes = lvl,
                       label = lvl,
                       shape = rep(c("box", "ellipse"), nn),
                       color = viridis(length(levels(col))+1)[col[1:length(lvl)]],
                       group = col[1:length(lvl)],
                       style = "filled",
                       fontname = "Roboto",
                       fontcolor = 'white',
                       shadow = T)

    # We can plot the graph directly
      visNetwork::visNetwork(nodes = node, edges = edge, ...) %>%
      visNetwork::visOptions(highlightNearest = TRUE) %>%
        visNetwork::visNodes(font = list(color = "white"), size = size)
  }
