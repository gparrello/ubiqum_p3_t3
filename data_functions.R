find_top_waps <- function(x, names = FALSE, k = 1){
  
  # from:
  # - https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
  # - https://stackoverflow.com/questions/17619782/how-to-find-the-largest-n-elements-in-a-list-in-r
  
  x <- sort(x, na.last = TRUE, decreasing = TRUE)
  if (names == TRUE) x <- names(x)
  x <- x[1:k]
  return(x)

}

remove_missing <- function(df){
  
  # Remove columns (WAP) where all the values = 0 (WAP was not detected)
  uniquelength <- sapply(df, function(x) length(unique(x)))
  df <- subset(df, select = uniquelength>1)
  
  # Remove rows (WAP) where all the values = 0 (WAP was not detected)
  keep <- apply(df[,1:183], 1, function(x) length(unique(x[!is.na(x)])) != 1)
  df[keep, ]

}