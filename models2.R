pacman::p_load(
  "caret",
  "doParallel"
)

set.seed(123)
source("./data.R")
source("./model_functions.R")
source("./data_functions.R")


do_training <- function(label, y, added_predictor, orig_sample){
  
  # Prepare data
  sample <- make_partition(orig_sample, orig_sample[[y]])
  train <- sample[["train"]]
  validation <- sample[["test"]]
  
  # Modeling
  data <- train
  predictors <- get_predictors(data, added_predictor)
  model <- save_model(label, data[predictors], data[[y]])
  
  # Get metrics
  data <- validation
  predictors <- get_predictors(data, added_predictor)
  metric <- get_metrics(data[predictors], model, data[[y]])
  
  return(metric)
  
}

sample_size <- 958
orig_sample <- dt[["common"]] %>%
  group_by(BUILDINGID, FLOOR) %>%
  sample_n(sample_size)

metrics <- c()

labels <- c(
  "building",
  "floor",
  "long",
  "lat",
  "long2"
)
predicted <- c(
  "BUILDINGID",
  "FLOOR",
  "LONGITUDE",
  "LATITUDE",
  "LONGITUDE"
)
added_predictors <- list(
  c(),
  c("BUILDINGID"),
  c("BUILDINGID", "FLOOR"),
  c("BUILDINGID", "FLOOR", "LONGITUDE"),
  c("BUILDINGID", "FLOOR", "LATITUDE")
)
names(predicted) <- labels
names(added_predictors) <- labels

# Do the magic
# cores <- detectCores() - 2
# cluster <- makeCluster(cores, type = "FORK")
# registerDoParallel(cluster)
for(l in labels){
  metrics[[l]] <- do_training(
    l,
    predicted[[l]],
    added_predictors[[l]],
    orig_sample
  )
}
# stopCluster(cluster)