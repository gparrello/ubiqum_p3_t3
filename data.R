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
  
  # convert attributes to factor
  factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")
  d <- d[, (523:528) := lapply(.SD, as.factor), .SDcols=factors]
  # d <- d[, factors := lapply(.SD, as.factor), .SDcols=factors]
  
  # remove rows and columns with 0 variance
  d <- remove_novar(d)
  
  # melt into long format
  # melt_ids <- colnames(d[,521:ncol(d)])
  # l <- melt(d, id.vars = melt_ids)
  # names(l)[names(l) == 'variable'] <- 'WAP'
  # longdt[[s]] <- l

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

common_columns <- intersect(
  colnames(dt[["test"]]),
  colnames(dt[["train"]])
)
dt[["train"]] <- dt[["train"]][,..common_columns]
dt[["common"]] <- rbind(dt[["train"]], dt[["test"]][,..common_columns])