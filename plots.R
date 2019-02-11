pacman::p_load(
  "data.table",
  "ggplot2",
  "ggridges"
)

source("./data.R")
source("./functions.R")
df <- dt[["train"]]
p <- ggplot(data = df) +
  aes(x = LONGITUDE, y = LATITUDE, color = TIMESTAMP) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

l <- longdt[["train"]]

waps <- unique(l$WAP)
n <- ceiling(length(waps)/25)
ridges <- c()
for(i in 1:n){
  lowest_x <- min(l[which(!is.na(l$value)),]$value)
  highest_x <- max(l[which(!is.na(l$value)),]$value)
  first_wap <- 1+(i-1)*25
  last_wap <- 25+(i-1)*25
  waps_subset <- waps[first_wap:last_wap]
  waps_subset <- waps_subset[!is.na(waps_subset)]
  ridges[[i]] <- ggplot(subset(l, WAP %in% waps_subset), aes(x = value, y = WAP)) +
    geom_density_ridges(scale = 10) +
    xlim(lowest_x,highest_x) +
    facet_wrap(vars(BUILDINGID))
}
# rm(l, waps, n)

lowest_x <- min(l[which(!is.na(l$value)),]$value)
highest_x <- max(l[which(!is.na(l$value)),]$value)
r <- ggplot(l, aes(x = value, y = WAP)) +
  geom_density_ridges(scale = 30) +
  theme_ridges() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  # theme(panel.spacing.x = 10) +
  xlim(lowest_x,highest_x) +
  facet_wrap(vars(BUILDINGID))
  # coord_fixed(2)
# h <- f_plot_hist(dt[["train"]])

