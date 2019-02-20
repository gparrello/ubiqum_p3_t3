pacman::p_load(
  "caret"
)

set.seed(123)
source("./data.R")
source("./model_functions.R")
source("./data_functions.R")

sample_size <- 958
orig_sample <- dt[["common"]] %>%
  group_by(BUILDINGID, FLOOR) %>%
  sample_n(sample_size)

metrics <- c()


#### Predicting building ####
sample <- make_partition(orig_sample, orig_sample$BUILDINGID)
train <- sample[["train"]]
validation <- sample[["test"]]

# Modeling
data <- train
predictors <- get_predictors(data)
model <- save_model("building", data[predictors], data$BUILDINGID)

# Get metrics
data <- validation
predictors <- get_predictors(data)
metrics[["building"]] <- get_metrics(data[predictors], model, data$BUILDINGID)


#### Predicting floor ####
sample <- make_partition(orig_sample, orig_sample$FLOOR)
train <- sample[["train"]]
validation <- sample[["test"]]

# Modeling
data <- train
predictors <- get_predictors(data, c("BUILDINGID"))
model <- save_model("floor", data[predictors], data$FLOOR)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID"))
metrics[["floor"]] <- get_metrics(data[predictors], model, data$FLOOR)


#### Predicting longitude ####
sample <- make_partition(orig_sample, orig_sample$LONGITUDE)
train <- sample[["train"]]
validation <- sample[["test"]]

# Modeling
data <- train
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR"))
model <- save_model("long", data[predictors], data$LONGITUDE)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR"))
metrics[["long"]] <- get_metrics(data[predictors], model, data$LONGITUDE)


#### Predicting latitude ####
sample <- make_partition(orig_sample, orig_sample$LATITUDE)
train <- sample[["train"]]
validation <- sample[["test"]]

# Modeling
data <- train
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LONGITUDE"))
model <- save_model("lat", data[predictors], data$LATITUDE)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LONGITUDE"))
metrics[["lat"]] <- get_metrics(data[predictors], model, data$LATITUDE)


#### Predicting longitude again ####
sample <- make_partition(orig_sample, orig_sample$LONGITUDE)
train <- sample[["train"]]
validation <- sample[["test"]]

# Modeling
data <- train
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LATITUDE"))
model <- save_model("long2", data[predictors], data$LONGITUDE)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LATITUDE"))
metrics[["long2"]] <- get_metrics(data[predictors], model, data$LONGITUDE)


# for future use
do_training <- function(label, orig_sample, y, added_predictors){
  
  # Prepared data
  sample <- make_partition(orig_sample, orig_sample[[y]])
  train <- sample[["train"]]
  validation <- sample[["test"]]
  
  # Modeling
  data <- train
  predictors <- get_predictors(data, added_predictors)
  model <- save_model(label, data[predictors], data[[y]])
  
  # Get metrics
  data <- validation
  predictors <- get_predictors(data, added_predictors)
  metrics[[label]] <- get_metrics(data[predictors], model, data[[y]])
  
}