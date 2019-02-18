find_top_waps <- function(x, names = FALSE, k = 1){
  
  # from:
  # - https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
  # - https://stackoverflow.com/questions/17619782/how-to-find-the-largest-n-elements-in-a-list-in-r
  
  x <- sort(x, na.last = TRUE, decreasing = TRUE)
  if (names == TRUE) x <- names(x)
  x <- x[1:k]
  return(x)

}