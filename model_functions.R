do_training <- function(data, y){
  
  # Train Control
  cvFoldNum <- 10
  cvRepeatNum <- 5
  cvFolds <- createMultiFolds(
    data[,y],
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
    data[,y],
    ntreeTry = 100,
    stepFactor = 2,
    improve = 0.05,
    trace = TRUE,
    plot = FALSE
  )
  
  # Train a random forest using that mtry
  model <- randomForest(
    y = data[,y],
    x = data[waps],
    importance = TRUE,
    method = "rf",
    ntree = 100,
    mtry = bestmtry[[1]],
    trControl = fitControl
  )
  
  return(model)
}