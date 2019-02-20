pacman::p_load(
  "readr",
  "caret",
  "randomForest",
  "e1071",
  "gbm",
)

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
  model[["rf"]] <- randomForest(
    x = x,
    y = y,
    importance = TRUE,
    method = "rf",
    ntree = 100,
    mtry = bestmtry[[1]],
    trControl = fitControl
  )
  
  # Train a svm
  # model[["svm"]] <- svm(y = y, x = x, trControl = fitControl)
  
  # Train a gbm
  # model[["gbm"]] <- gbm(x = x, y = y)
  
  return(model)

}


get_metrics <- function(data, model, real){
  
  metric <- c()
  for(m in names(model)){
    waps <- grep("WAP", names(data), value = TRUE)
    predicted <- predict(model[[m]], data[,..dependents])
    metric[[m]] <- postResample(predicted, real)
  }
  
  return(metric)

}

save_model <- function(label, x, y){
  
  get_time <- function(){
    t <- round(as.numeric(as.POSIXct(Sys.time())), 0)
    return(t)
  }
  
  start_time <- get_time()
  model <- do_modeling(x, y)
  end_time <- get_time()
  
  filename <- paste(
    "./models/",
    label,
    "/",
    start_time,
    "_",
    end_time,
    ".rba",
    sep=""
  )
  
  save(model, file = filename)
  
  return(model)
}