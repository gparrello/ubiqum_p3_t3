pacman::p_load(
  "randomForest",
  "caret"
)

source("./data.R")

data <- dt[["train"]]

sample_size <- 200
sample <- data %>% group_by(BUILDINGID) %>% sample_n(sample_size)

trainsize <- .75
intraining <- createDataPartition(
  sample$BUILDINGID,
  p = trainsize,
  list = FALSE
)
training <- sample[intraining, ]
testing <- sample[-intraining, ]

#### Train Control ####
cvFoldNum <- 10
cvRepeatNum <- 5
cvFolds <- createMultiFolds(
  training$BUILDINGID,
  k = cvFoldNum,
  times = cvRepeatNum
)

fitControl <- trainControl(
  method = "repeatedcv",
  index = cvFolds
)

# Saving the waps in a vector
waps <- grep("WAP", names(training), value = TRUE)

# Predicting building #

# Get the best mtry
bestmtry <- tuneRF(
  training[waps],
  training$BUILDINGID,
  ntreeTry = 100,
  stepFactor = 2,
  improve = 0.05,
  trace = TRUE,
  plot = FALSE
)

# Train a random forest using that mtry
model <- randomForest(
  y = training$BUILDINGID,
  x = training[waps],
  importance = TRUE,
  method = "rf",
  ntree = 100,
  mtry = bestmtry[[1]],
  trControl = fitControl
)

training$BUILDINGID_ <- predict(rf, training[waps])
postResample(training$BUILDINGID_, training$BUILDINGID)