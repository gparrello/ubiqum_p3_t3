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
c <- unique(l$WAP)
n <- ceiling(length(c)/25)
ridges <- c()
for(i in 1:n){
  m1 <- min(l[which(!is.na(l$value)),]$value)
  m2 <- max(l[which(!is.na(l$value)),]$value)
  lower <- 1+(i-1)*25
  upper <- 25+(i-1)*25
  csub <- c[lower:upper]
  csub <- csub[!is.na(csub)]
  ridges[[i]] <- ggplot(subset(l, WAP %in% csub), aes(x = value, y = WAP)) +
    geom_density_ridges(scale = 10) +
    xlim(m1,m2) +
    facet_wrap(vars(BUILDINGID))
}
# rm(l, c, n)

m1 <- min(l[which(!is.na(l$value)),]$value)
m2 <- max(l[which(!is.na(l$value)),]$value)
r <- ggplot(l, aes(x = value, y = WAP)) +
  geom_density_ridges(scale = 30) +
  theme_ridges() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  # theme(panel.spacing.x = 10) +
  xlim(m1,m2) +
  facet_wrap(vars(BUILDINGID))
  # coord_fixed(2)
# h <- fPlotHist(dt[["train"]])

