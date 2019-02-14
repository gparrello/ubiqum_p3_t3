find_top_waps <- function(x, names = FALSE, k = 1){
  x <- order(x, na.last = TRUE, decreasing = TRUE)[1:k]
  if (names == TRUE) x <- names(x)
  return(x)
}