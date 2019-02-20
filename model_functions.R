pacman::p_load(
  # "readr",
  "doParallel",
  "caret",
  "xgboost",
  "randomForest",
  "e1071",
  "gbm"
)

do_modeling <- function(x, y, caret = FALSE){

  model <- c()
  cores <- detectCores() - 2
  cluster <- makeCluster(cores, type = "FORK")
  registerDoParallel(cluster)
  
  
  # Train Control
  cvFoldNum <- 10
  cvRepeatNum <- 3
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
  
  if (caret == TRUE) {
      
    # Train
    tuneLength <- 1
    allmodels <- list(
      # "svmLinear",
      # "svmRadial",
      # "svmPoly",
      # "lm",
      # "knn",
      # "gbm",
      # "xgbTree",
      "rf"
    )
    
    for(m in allmodels){
      model[[m]] <- train(
        x = x,
        y = y,
        # data = trainingSet, # the problem is here!
        method = m,
        trControl = fitControl,
        tuneLength = tuneLength
      )
    }
  
  } else {
    
    # Train RF
    bestmtry <- tuneRF(
      x, y,
      ntreeTry = 100,
      stepFactor = 2,
      improve = 0.05,
      trace = TRUE,
      plot = FALSE,
      trControl = fitControl
    )
    
    model[["rf"]] <- randomForest(
      y = y,
      x = x,
      importance = TRUE,
      method = "rf",
      ntree = 100,
      mtry = bestmtry[[1]],
      trControl = fitControl
    )

  }
  
  stopCluster(cluster)
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