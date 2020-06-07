checksignals <- function(x){
  all(x>0) || all(x<0)
}

read_prior <- function(prior){
  aux <- unlist(strsplit(prior, "\\("))
  dist <- aux[1]
  aux2 <- unlist(strsplit(aux[2], "\\)"))[1]
  val <- unlist(strsplit(aux2, "\\,"))
  return(c(dist, val))
}
