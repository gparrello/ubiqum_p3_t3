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
  
  # set 100 to NAs/-105
  for (j in seq_along(d[,1:520])) {
    set(d, i = which(d[[j]] == 100), j = j, value = -105)
    # set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }
  
  factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")
  d <- d[, (523:528) := lapply(.SD, as.factor), .SDcols=factors]
  
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

  # check_list[[s]] <- apply(longdt[[s]][, c(3:8, 10)], 2, unique)
  
  # add top WAPs columns
  n <- 1
  for (k in 1:n) {
    tops <- apply(d[,1:520], 1, find_top_waps, k = k, names = FALSE)
    tops_names <- apply(d[,1:520], 1, find_top_waps, k = k, names = TRUE)
    d <- cbind(d, tops)
    d <- cbind(d, tops_names)
    rm(tops, tops_names)
  }
  rm(n)
  
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
# }


train <- dt[["train"]]
sample <- train %>% group_by(BUILDINGID, FLOOR) %>% sample_n(10)

# Load package
library(randomForest)
library(caret)

# Saving the waps in a vector
WAPs <- grep("WAP", names(train), value=T)

# Get the best mtry
bestmtry_rf <- tuneRF(
  sample[WAPs],
  sample$BUILDINGID,
  ntreeTry = 100,
  stepFactor = 2,
  improve = 0.05,
  trace = TRUE,
  plot = TRUE
)

# Train a random forest using that mtry
system.time(
  rf_reg <- randomForest(
    y = sample$LONGITUDE,
    x = sample[WAPs],
    importance = TRUE,
    method = "rf",
    ntree = 100,
    mtry = bestmtry_rf[[1]]
  )
)

# Train a random forest using caret package
system.time(
  rf_reg_caret <- train(
    y = sample$LONGITUDE,
    x = sample[WAPs],
    data = sample,
    method = "rf",
    ntree = 100,
    tuneGrid = expand.grid(.mtry=bestmtry_rf[[1]])
  )
)
