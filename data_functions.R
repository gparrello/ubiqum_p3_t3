find_top_waps <- function(x, names = FALSE, k = 1){
  x <- sort(x, na.last = TRUE, decreasing = TRUE)
  if (names == TRUE) x <- names(x)
  x <- x[1:k]
  return(x)
}