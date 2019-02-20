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
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
predictors <- waps
model <- save_model("building", data[predictors], data$BUILDINGID)

# Get metrics
data <- validation
metrics[["building"]] <- get_metrics(validation, model, data$BUILDINGID)


#### Predicting floor ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID")
model <- save_model("floor", data[predictors], data$FLOOR)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID")
metrics[["floor"]] <- get_metrics(data[,..predictors], model, data$FLOOR)


#### Predicting longitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID", "FLOOR")
model <- save_model("long", data[predictors], data$LONGITUDE)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID", "FLOOR")
metrics[["long"]] <- get_metrics(data[,..predictors], model, data$LONGITUDE)


#### Predicting latitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID", "FLOOR", "LONGITUDE")
model <- save_model("lat", data[predictors], data$LATITUDE)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
predictors <- c(waps, "BUILDINGID", "FLOOR", "LONGITUDE")
metrics[["lat"]] <- get_metrics(data[,..predictors], model, data$LATITUDE)
