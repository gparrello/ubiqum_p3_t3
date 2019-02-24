source("./data.R")

data <- fread("./data/testData.csv")
waps <- grep("WAP", names(data), value=TRUE)

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
  "LONGITUDE"
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
  # browser()
  if (!(prod(dim(prediction)) == 0)) {
    prediction <- cbind(prediction, p)
  } else {
    prediction <- p
  }
}
