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
    geom_density_ridges() +
    xlim(m1,m2) +
    facet_wrap(vars(BUILDINGID))
}
rm(l, c, n)

# h <- fPlotHist(dt[["train"]])

