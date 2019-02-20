pacman::p_load(
  "caret"
)

set.seed(123)
source("./data.R")
source("./model_functions.R")

sample_size <- 300
sample <- dt[["train"]] %>%
  group_by(BUILDINGID, FLOOR) %>%
  sample_n(sample_size)

common_columns <- intersect(
  colnames(dt[["test"]]),
  colnames(dt[["train"]])
)
validation <- dt[["test"]][,..common_columns]

metrics <- c()


#### Predicting building ####

# Modeling
data <- sample
predictors <- get_predictors(data)
model <- save_model("building", data[predictors], data$BUILDINGID)

# Get metrics
data <- validation
predictors <- get_predictors(data)
metrics[["building"]] <- get_metrics(data[,..predictors], model, data$BUILDINGID)


#### Predicting floor ####

# Modeling
data <- sample
predictors <- get_predictors(data, c("BUILDINGID"))
model <- save_model("floor", data[predictors], data$FLOOR)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID"))
metrics[["floor"]] <- get_metrics(data[,..predictors], model, data$FLOOR)


#### Predicting longitude ####

# Modeling
data <- sample
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR"))
model <- save_model("long", data[predictors], data$LONGITUDE)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR"))
metrics[["long"]] <- get_metrics(data[,..predictors], model, data$LONGITUDE)


#### Predicting latitude ####

# Modeling
data <- sample
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LONGITUDE"))
model <- save_model("lat", data[predictors], data$LATITUDE)

# Get metrics
data <- validation
predictors <- get_predictors(data, c("BUILDINGID", "FLOOR", "LONGITUDE"))
metrics[["lat"]] <- get_metrics(data[,..predictors], model, data$LATITUDE)
