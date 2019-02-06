pacman::p_load(
  "data.table"
)

trainingFile <- "./data/trainingData.csv"
testingFile <- "./data/validationData.csv"
dt <- fread(trainingFile)
# df <- read.csv(trainingFile)
