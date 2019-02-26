pacman::p_load(
  "dplyr"
)

get_data <- function(x){
  df <- read.csv(file=paste(folder, x, sep=""))
  if (df$X[[1]] == 0) df$X <- df$X + 1
  df$X <- as.integer(df$X)
  df$FLOOR <- as.factor(df$FLOOR)
  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  df$LATITUDE <- as.numeric(df$LATITUDE)
  df <- df[c("X", "FLOOR", "LONGITUDE", "LATITUDE")]
  df$who <- x
  return(df)
}

get_mode <- function(v) {
  uniqv <- unique(v)
  mode <- uniqv[which.max(tabulate(match(v, uniqv)))]
  return(mode)
}

folder <- "./experiment/"
prediction_files <- list.files(folder)
list_df <- lapply(
  prediction_files,
  get_data
)
names(list_df) <- prediction_files
binded_df <- do.call(rbind, list_df)
binded_df$FLOOR <- as.factor(binded_df$FLOOR)

grouped_df <- binded_df %>%
  group_by(X) %>%
  summarise(
    FLOOR = get_mode(FLOOR),
    LONGITUDE = mean(LONGITUDE),
    LATITUDE = mean(LATITUDE)
  )

write.csv(grouped_df, file = paste(folder, "final.csv"), row.names = FALSE)
grouped_df$who <- "combined"
full_df <- rbind(binded_df, grouped_df)

ggplot(data = full_df) +
  aes(x = LATITUDE, fill = who) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_grid(cols=vars(who))

ggplot(data = full_df) +
  aes(x = LONGITUDE, fill = who) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_grid(cols=vars(who))

ggplot(data = full_df) +
  aes(x = FLOOR, fill = who) +
  geom_bar() +
  theme_minimal() +
  facet_grid(cols=vars(who))

ggplot(data = full_df) +
  aes(x = LONGITUDE, y = LATITUDE, color = who) +
  geom_point() +
  theme_minimal() +
  facet_grid(cols=vars(FLOOR), rows=vars(who))

ggplot(data = full_df) +
  aes(x = LONGITUDE, y = LATITUDE, color = who) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

# library("rbokeh")
# figure() %>%
#   ly_points(
#     data = full_df,
#     x = LONGITUDE,
#     y = LATITUDE,
#     color = who
#   )
