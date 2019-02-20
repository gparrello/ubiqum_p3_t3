pacman::p_load(
  "readr",
  "caret",
  "randomForest",
  "e1071",
  "gbm",
  "doParallel"
)

do_modeling <- function(x, y){

  model <- c()
  cl <- makeCluster(detectCores() - 2)
  registerDoParallel(cl)
  
  
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
    index = cvFolds,
    allowParallel = TRUE
  )
  
  # Get the best mtry
  bestmtry <- tuneRF(
    x,
    y,
    ntreeTry = 100,
    stepFactor = 2,
    improve = 0.05,
    trace = TRUE,
    plot = FALSE,
    trControl = fitControl
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
  
  
  stopCluster(cl)
  return(model)

}


get_metrics <- function(predictors, model, real){
  
  metric <- c()
  for(m in names(model)){
    predicted <- predict(model[[m]], predictors)
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

get_predictors <- function(data, predictors = c()){
  
  waps <- grep("WAP", names(data), value = TRUE)
  predictors <- c(waps, predictors)  
  
  return(predictors)

}