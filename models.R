pacman::p_load(
  "randomForest",
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
dependents <- waps
model <- do_modeling(data[dependents], data$BUILDINGID)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
metrics[["building"]] <- postResample(predict(model, data[,..dependents]), data$BUILDINGID)


#### Predicting floor ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID")
model <- do_modeling(data[dependents], data$FLOOR)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
metrics[["floor"]] <- postResample(predict(model, data[,..dependents]), data$FLOOR)


#### Predicting longitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID", "FLOOR")
model <- do_modeling(data[dependents], data$LONGITUDE)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
metrics[["long"]] <- postResample(predict(model, data[,..dependents]), data$LONGITUDE)


#### Predicting latitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID", "FLOOR", "LONGITUDE")
model <- do_modeling(data[dependents], data$LATITUDE)

# Get metrics
data <- validation
waps <- grep("WAP", names(data), value = TRUE)
metrics[["lat"]] <- postResample(predict(model, data[,..dependents]), data$LATITUDE)
