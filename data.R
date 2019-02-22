pacman::p_load(
  "data.table",
  "dplyr"
)

source("./data_functions.R")

files <- c(
  train = "./data/trainingData.csv",
  validation = "./data/validationData.csv",
  test2 = "./data/testData.csv"
)

dt <- c()
longdt <- c()
check_list <- c()
factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")

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
  d <- d[, (factors) := lapply(.SD, as.factor), .SDcols=factors]
  
  # remove rows and columns with 0 variance
  d <- remove_novar(d)
  
  # melt into long format
  waps <- grep("WAP", names(d), value=TRUE)
  melt_ids <- colnames(d[,(length(waps)+1):ncol(d)])
  l <- melt(d, id.vars = melt_ids)
  names(l)[names(l) == 'variable'] <- 'WAP'
  longdt[[s]] <- l

  # weight signal according to phone brand
  
      
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
  d$tops_names <- as.factor(d$tops_names)
  
  dt[[s]] <- d
  
  # rm(d, l)
  # rm(keeprows, uniquelength)
}

common_columns <- intersect(
  colnames(dt[["validation"]]),
  colnames(dt[["train"]])
)
dt[["common"]] <- rbind(
  dt[["train"]][,..common_columns],
  dt[["validation"]][,..common_columns]
)