
library(anticlust)
library(ggplot2)

oasis <- read.csv("https://raw.githubusercontent.com/aenneb/OASIS-beauty/master/means_per_image.csv")

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

features <- subset(oasis, select = c(beauty_mean, Valence_mean, Arousal_mean))
extended <- cbind(features, squared_from_mean(features))

oasis$ac <- anticlustering(
  scale(extended),
  K = 10,
  categories = oasis$Category, 
  objective = "variance",
  method = "local-maximum"
)

mean_sd_tab(features, oasis$ac)

table(ac, oasis$Category)
