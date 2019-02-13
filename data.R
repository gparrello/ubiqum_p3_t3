pacman::p_load(
  "data.table",
  "dplyr"
)

files <- c(
  train = "./data/trainingData.csv",
  test = "./data/validationData.csv"
)
dt <- c()
longdt <- c()
check_list <- c()
for (s in names(files)) {
  
  d <- fread(files[[s]])
  
  # set 100 to NAs
  for (j in seq_along(d[,1:520])) {
    set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }   
  
  # melt into long format
  melt_ids <- colnames(d[,521:529])
  l <- melt(d, id.vars = melt_ids)
  names(l)[names(l) == 'variable'] <- 'WAP'
  longdt[[s]] <- l
  
  check_list[[s]] <- apply(longdt[[s]][, c(3:8, 10)], 2, unique)
  
  # remove all rows that only contain +100 in WAP signal
  # keeprows <- apply(dt[[s]][,1:520], 1, function(x) length(unique(x[!is.na(x)])) != 1)  # criteria for selecting 100 and not all rows with all -40 for example?
  # dt[[s]] <- dt[[s]][keeprows,]
  
  # remove all columns that only contain +100 in WAP signal
  # uniquelength <- sapply(dt[[s]],function(x) length(unique(x)))
  # dt[[s]] <- subset(dt[[s]], select= uniquelength > 1)

  dt[[s]] <- d
  
  rm(d, l)
  # rm(keeprows, uniquelength)
}

 
# for (c in names(check_list[["train"]])) {
#   print(paste("in column", c))
#   print("in training set but not in testing set:")
#   print(setdiff(check_list[["train"]][[c]], check_list[["test"]][[c]]))
#   print("in testing set but not in training set:")
#   print(setdiff(check_list[["test"]][[c]], check_list[["train"]][[c]]))
  # browser()
# }

