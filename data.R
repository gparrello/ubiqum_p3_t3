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
  waps <- grep("WAP", names(d), value=TRUE)
  
  # set 100 to NAs/-105
  for (j in seq_along(d[, ..waps])) {
    set(d, i = which(d[[j]] == 100), j = j, value = -105)
    # set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }
  
  factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")
  d <- d[, (523:528) := lapply(.SD, as.factor), .SDcols=factors]
  # d <- d[, factors := lapply(.SD, as.factor), .SDcols=factors]
  
  # d$NEWID <- as.factor(paste(
  #   d$BUILDINGID,".",
  #   d$FLOORID,
  #   sep = ""
  # ))
  
  d <- remove_missing(d)
  
  # melt into long format
  # melt_ids <- colnames(d[,521:ncol(d)])
  # l <- melt(d, id.vars = melt_ids)
  # names(l)[names(l) == 'variable'] <- 'WAP'
  # longdt[[s]] <- l

  # check_list[[s]] <- apply(longdt[[s]][, c(3:8, 10)], 2, unique)
  
  # add top WAPs columns
  n <- 1
  for (k in 1:n) {
    waps <- grep("WAP", names(d), value=TRUE)
    tops <- apply(d[,..waps], 1, find_top_waps, k = k, names = FALSE)
    tops_names <- apply(d[,..waps], 1, find_top_waps, k = k, names = TRUE)
    d <- cbind(d, tops)
    d <- cbind(d, tops_names)
    rm(tops, tops_names)
  }
  rm(n)
  
  dt[[s]] <- d
  
  # rm(d, l)
  # rm(keeprows, uniquelength)
}

 
# for (c in names(check_list[["train"]])) {
#   print(paste("in column", c))
#   print("in training set but not in testing set:")
#   print(setdiff(check_list[["train"]][[c]], check_list[["test"]][[c]]))
#   print("in testing set but not in training set:")
#   print(setdiff(check_list[["test"]][[c]], check_list[["train"]][[c]]))
# }

common_columns <- intersect(
  colnames(dt[["test"]]),
  colnames(dt[["train"]])
)
dt[["train"]] <- dt[["train"]][,..common_columns]
dt[["common"]] <- rbind(dt[["train"]], dt[["test"]][,..common_columns])