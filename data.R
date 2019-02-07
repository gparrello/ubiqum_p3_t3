pacman::p_load(
  "data.table",
  "dplyr"
)

files <- c(
  train = "./data/trainingData.csv",
  test = "./data/validationData.csv"
)
dt <- c()
longdt <- c()
for(s in names(files)){
  
  d <- fread(files[[s]])
  
  for(j in seq_along(d[,1:520])) {
    set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }   
  
  meltIds <- colnames(d[,521:529])
  l <- melt(d, id.vars = meltIds)
  names(l)[names(l) == 'variable'] <- 'WAP'
  longdt[[s]] <- l
  
  # remove all rows that only contain +100 in WAP signal
  # keeprows <- apply(dt[[s]][,1:520], 1, function(x) length(unique(x[!is.na(x)])) != 1)  # criteria for selecting 100 and not all rows with all -40 for example?
  # dt[[s]] <- dt[[s]][keeprows,]
  
  # remove all columns that only contain +100 in WAP signal
  # uniquelength <- sapply(dt[[s]],function(x) length(unique(x)))
  # dt[[s]] <- subset(dt[[s]], select= uniquelength > 1)

  dt[[s]] <- d
  
  rm(d, l)
  # rm(keeprows, uniquelength)
}
