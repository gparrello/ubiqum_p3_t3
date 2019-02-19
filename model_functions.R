do_modeling <- function(x, y){

  # Train Control
  cvFoldNum <- 10
  cvRepeatNum <- 5
  cvFolds <- createMultiFolds(
    y,
    k = cvFoldNum,
    times = cvRepeatNum
  )
  
  fitControl <- trainControl(
    method = "repeatedcv",
    index = cvFolds
  )
  
  # Get the best mtry
  bestmtry <- tuneRF(
    x,
    y,
    ntreeTry = 100,
    stepFactor = 2,
    improve = 0.05,
    trace = TRUE,
    plot = FALSE
  )
  
  # Train a random forest using that mtry
  model <- randomForest(
    x = x,
    y = y,
    importance = TRUE,
    method = "rf",
    ntree = 100,
    mtry = bestmtry[[1]],
    trControl = fitControl
  )
  
  return(model)

}