pacman::p_load(
  # "readr",
  "caret",
  "xgboost",
  "randomForest",
  "e1071",
  "gbm"
)

do_modeling <- function(x, y, caret = FALSE){

  model <- c()
  
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
  
  return(model)

}


get_metrics <- function(predictors, model, real){
  
  metrics <- data.frame()
  for(m in names(model)){
    predicted <- predict(model[[m]], predictors)
    metric <- as.data.frame(postResample(predicted, real))
    colnames(metric) <- "value"
    metric$model <- m
    metrics <- rbind(metrics, metric)
  }
  vars <- row.names(metrics)
  metrics <- melt(metrics)
  metrics$variable <- vars
  
  return(metrics)

}

get_time <- function(){
  t <- round(as.numeric(as.POSIXct(Sys.time())), 0)
  return(t)
}

save_model <- function(label, x, y){
  
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