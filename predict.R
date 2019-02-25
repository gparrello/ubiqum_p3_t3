source("./data.R")
source("./model_functions.R")

data <- fread("./data/testData.csv")
waps <- grep("WAP", names(data), value=TRUE)
for (j in seq_along(data[, ..waps])) {
  set(data, i = which(data[[j]] == 100), j = j, value = -105)
}

labels <- c(
  "building",
  "floor",
  "long",
  "lat",
  "long2"
)
variables <- c(
  "BUILDINGID",
  "FLOOR",
  "LONGITUDE",
  "LATITUDE",
  "LONGITUDE2"
)
names(variables) <- labels
models <- c()
prediction <- data.frame()

for (l in labels) {
  print(l)
  load(paste("./best/", l, ".rda", sep=""))
  if (!(prod(dim(prediction)) == 0)) {
    d <- cbind(data[,..waps], prediction)
  } else {
    d <- data[,..waps]
  }
  p <- predict(model$rf, d)
  p <- as.data.frame(p)
  colnames(p) <- variables[[l]]
  if (!(prod(dim(prediction)) == 0)) {
    prediction <- cbind(prediction, p)
  } else {
    prediction <- p
  }
}

prediction$LONGITUDE <- prediction$LONGITUDE2
prediction$LONGITUDE2 <- NULL

filename <- paste("./predictions/", get_time(), ".csv", sep="")
write.csv(prediction, file = filename)
