source("./data.R")

labels <- c(
  "building"#,
  # "floor",
  # "long",
  # "lat"
)
models <- c()

for (l in labels) {
  load(paste("./best/", l, ".rda", sep=""))
  models[[l]] <- model
}

common_columns <- Reduce(
  intersect, list(
    colnames(dt[["train"]]),
    colnames(dt[["validation"]]),
    colnames(dt[["test"]])
  )
)

data <- fread("./data/testData.csv")
waps <- grep("WAP", names(data), value=TRUE)
prediction <- predict(models[["building"]]$rf, data[,..waps])
