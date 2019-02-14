find_top_waps <- function(x, names = FALSE, k = 1){
  
  # from:
  # - https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
  # - https://stackoverflow.com/questions/17619782/how-to-find-the-largest-n-elements-in-a-list-in-r
  
  x <- order(x, na.last = TRUE, decreasing = TRUE)[1:k]
  if (names == TRUE) x <- names(x)
  return(x)
}