pacman::p_load(
  "data.table",
  "dplyr"
)

source("./data_functions.R")

files <- c(
  train = "./data/trainingData.csv",
  test = "./data/validationData.csv"
)

dt <- c()
longdt <- c()
check_list <- c()

# Processing both datasets
for (s in names(files)) {
  
  d <- fread(files[[s]])
  
  # set 100 to NAs
  for (j in seq_along(d[,1:520])) {
    set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }   
  
  factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")
  dtnew <- d[, (523:528) := lapply(.SD, as.factor), .SDcols=factors]
  # browser()
  
  # d$NEWID <- as.factor(paste(
  #   d$BUILDINGID,".",
  #   d$FLOORID,
  #   sep = ""
  # ))
  
  # remove all rows that only contain +100 in WAP signal
  # keeprows <- apply(dt[[s]][,1:520], 1, function(x) length(unique(x[!is.na(x)])) != 1)  # criteria for selecting 100 and not all rows with all -40 for example?
  # dt[[s]] <- dt[[s]][keeprows,]
  
  # remove all columns that only contain +100 in WAP signal
  # uniquelength <- sapply(dt[[s]],function(x) length(unique(x)))
  # dt[[s]] <- subset(dt[[s]], select= uniquelength > 1)
  
  # melt into long format
  melt_ids <- colnames(d[,521:ncol(d)])
  l <- melt(d, id.vars = melt_ids)
  names(l)[names(l) == 'variable'] <- 'WAP'
  longdt[[s]] <- l

  check_list[[s]] <- apply(longdt[[s]][, c(3:8, 10)], 2, unique)
  
  # add top WAPs columns
  # n <- 5
  # for (k in 1:n){
  #   
  # }
  
  # tops <- apply(d[,1:520], 1, find_top_waps, k=1, names=FALSE)
  # tops_names <- apply(d[,1:520], 1, find_top_waps, k=1, names=TRUE)
  # d <- cbind(d, tops)
  # d <- cbind(d, tops_names)
  
  
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

