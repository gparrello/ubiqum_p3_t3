pacman::p_load(
  "dplyr"
)

get_data <- function(x){
  df <- read.csv(file=paste(folder, x, sep=""))
  # df$who <- x
  if (df$X[[1]] == 0) df$X <- df$X + 1
  df$X <- as.integer(df$X)
  df$BUILDINGID <- as.factor(df$BUILDINGID)
  df$FLOOR <- as.factor(df$FLOOR)
  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  df$LATITUDE <- as.numeric(df$LATITUDE)
  df <- df[c("X", "BUILDINGID", "FLOOR", "LONGITUDE", "LATITUDE")]
  return(df)
}

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

folder <- "./experiment/"
prediction_files <- list.files(folder)
list_df <- lapply(
  prediction_files,
  get_data
)
names(list_df) <- prediction_files
binded_df <- do.call(rbind, list_df)
binded_df$BUILDINGID <- as.factor(binded_df$BUILDINGID)
binded_df$FLOOR <- as.factor(binded_df$FLOOR)
# browser()

grouped_df <- binded_df %>%
  group_by(X) %>%
  summarise(
    BUILDINGID_avg = get_mode(BUILDINGID),
    FLOOR_avg = get_mode(FLOOR),
    LONGITUDE_avg = mean(LONGITUDE),
    LATITUDE_avg = mean(LATITUDE)
  )

# write.csv(grouped_df, file = paste(folder, "final.csv"), row.names = FALSE)