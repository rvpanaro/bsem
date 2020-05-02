#' Array Plotting
#'
#' Graphical representation for matrix class objects using lattice package.
#'
#' @export arrayplot
#' @param matrix a numerical data.frame or matrix object.
#' @param colors 1 = blue/red, 0 = white/black.
#' @param mini,maxi color range.
#' @examples
#'
#' data(iris)
#'
#' plotMatrix(iris[, c(1,2,3)])
#' plotMatrix(iris[, c(1,2,3)], colors = 0)
#' @importFrom lattice levelplot panel.levelplot

arrayplot <-
  function(y,
           mini = -max(abs(min(y)), max(y)),
           maxi = max(abs(min(y)), max(y)),
           colors = 1,
           ylab = "",
           xlab = "",
           main = ""){

 # y: data matrix to be displayed
 # maxi: maximum value to be displayed with the highest color
 # colors: pattern of colors (1 = blue/red, 0 = white/black)

 y <- as.matrix(y)
 xl <- xlab ## x axis label
 yl <- ylab ## y axis label
 nr <- nrow(y)
 nc <- ncol(y)

 ##
 if(nr<=5){
   spr <- 1
 }
 else{
   spr <- round(nr/5)
 }
 if(nc<=5){
   spc <- 1
 }
 else{
   spc <- round(nc/5)
 }
 lab.x <- seq(1, nc, spc) #lab.x[length(lab.x)] = nc;
 lab.y <- seq(1, nr, spr) #lab.y[length(lab.y)] = nr;

 sc <- list( x = list(at = seq(1, nc, spc),
                    labels = as.character(lab.x),
                    cex = 1.5),
             y = list(at = seq(nr, 1, -spr),
                      labels=as.character(lab.y),
                      rot = 90,
                      cex = 1.0)
             )
 if(colors==1){
   col.l = colorRampPalette(c("blue","white","red"));
   aux.cor = "white"
 }
 if(colors==0){
   col.l = colorRampPalette(c("white","gray","black"));
   aux.cor = "gray"
 }

 cbar <- seq(mini, maxi, length.out = 100);
 ckey <- list(labels = list(cex = 1.5))

 levelplot(
   t(y[nr:1,]),
   panel = function(...){panel.levelplot(...) },
   col.regions=col.l,
   xlab=list(xl, cex = 1.5),
   ylab=list(yl, cex = 1.5),
   scales=sc, at = cbar,
   colorkey = ckey,
   aspect="fill", main = main
   )
}
