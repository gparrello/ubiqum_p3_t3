pacman::p_load(
  "randomForest",
  "caret"
)

source("./data.R")

#### Predicting building ####
# Prepare data
data <- dt[["train"]]
sample_size <- 200
sample <- data %>% group_by(BUILDINGID) %>% sample_n(sample_size)
# trainsize <- .75
# indata <- createDataPartition(
#   sample$BUILDINGID,
#   p = trainsize,
#   list = FALSE
# )
# training <- sample[indata, ]
# testing <- sample[-indata, ]
data <- sample

# Train Control
cvFoldNum <- 10
cvRepeatNum <- 5
cvFolds <- createMultiFolds(
  data$BUILDINGID,
  k = cvFoldNum,
  times = cvRepeatNum
)

fitControl <- trainControl(
  method = "repeatedcv",
  index = cvFolds
)

# Saving the waps in a vector
waps <- grep("WAP", names(data), value = TRUE)


# Get the best mtry
bestmtry <- tuneRF(
  data[waps],
  data$BUILDINGID,
  ntreeTry = 100,
  stepFactor = 2,
  improve = 0.05,
  trace = TRUE,
  plot = FALSE
)

# Train a random forest using that mtry
model <- randomForest(
  y = data$BUILDINGID,
  x = data[waps],
  importance = TRUE,
  method = "rf",
  ntree = 100,
  mtry = bestmtry[[1]],
  trControl = fitControl
)

# Get metrics
common_columns <- intersect(
  colnames(dt[["test"]]),
  colnames(dt[["train"]])
)
data <- dt[["test"]][,..common_columns]
waps <- grep("WAP", names(data), value = TRUE)
data$BUILDINGID_ <- predict(model, data[,..waps])
postResample(data$BUILDINGID_, data$BUILDINGID)



#### Predicting floor ####
# Prepare data
data <- dt[["train"]]
sample_size <- 200
sample <- data %>% group_by(FLOOR) %>% sample_n(sample_size)
data <- sample

# Train Control
cvFoldNum <- 10
cvRepeatNum <- 5
cvFolds <- createMultiFolds(
  data$FLOOR,
  k = cvFoldNum,
  times = cvRepeatNum
)

fitControl <- trainControl(
  method = "repeatedcv",
  index = cvFolds
)

# Saving the waps in a vector
waps <- grep("WAP", names(data), value = TRUE)


# Get the best mtry
bestmtry <- tuneRF(
  data[waps],
  data$FLOOR,
  ntreeTry = 100,
  stepFactor = 2,
  improve = 0.05,
  trace = TRUE,
  plot = FALSE
)

# Train a random forest using that mtry
model <- randomForest(
  y = data$FLOOR,
  x = data[waps],
  importance = TRUE,
  method = "rf",
  ntree = 100,
  mtry = bestmtry[[1]],
  trControl = fitControl
)

# Get metrics
common_columns <- intersect(
  colnames(dt[["test"]]),
  colnames(dt[["train"]])
)
data <- dt[["test"]][,..common_columns]
waps <- grep("WAP", names(data), value = TRUE)
data$FLOOR_ <- predict(model, data[,..waps])
postResample(data$FLOOR_, data$FLOOR)
