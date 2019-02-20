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
dependents <- waps
model <- save_model("building", data[dependents], data$BUILDINGID)

# Get metrics
data <- validation
metrics[["building"]] <- get_metrics(validation, model, data$BUILDINGID)


#### Predicting floor ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID")
model <- save_model("floor", data[dependents], data$FLOOR)

# Get metrics
data <- validation
metrics[["floor"]] <- get_metrics(validation, model, data$FLOOR)


#### Predicting longitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID", "FLOOR")
model <- save_model("long", data[dependents], data$LONGITUDE)

# Get metrics
data <- validation
metrics[["long"]] <- get_metrics(validation, model, data$LONGITUDE)


#### Predicting latitude ####
# Prepare data
data <- sample

# Modeling
waps <- grep("WAP", names(data), value = TRUE)
dependents <- c(waps, "BUILDINGID", "FLOOR", "LONGITUDE")
model <- save_model("lat", data[dependents], data$LATITUDE)

# Get metrics
data <- validation
metrics[["lat"]] <- get_metrics(validation, model, data$LATITUDE)


# try making unique labels with long+lat+floor and using
# the validation set too! it will give you very rich data because
# the validation set locations are not fixed