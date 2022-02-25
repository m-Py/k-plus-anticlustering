
library(anticlust)
library(DescTools) # for computing skew and kurtosis

oasis <- read.csv("https://raw.githubusercontent.com/aenneb/OASIS-beauty/master/means_per_image.csv")

# function to compute features for variance
# moment = 2 -> variance; 3 = skew; 4 = kurtosis
moment_features <- function(data, moment = 2) {
  apply(data, 2, function(x) (x - mean(x))^moment)
}

features <- subset(oasis, select = c(beauty_mean, Valence_mean, Arousal_mean))
extended <- cbind(features, moment_features(features))
extended_skew_kurtosis <- cbind(
  extended, 
  moment_features(features, 3),
  moment_features(features, 4)
)

# standard k-plus anticlustering
anticlusters_cov <- anticlustering(
  scale(extended),
  K = 3,
  objective = "variance"
)

# k-plus anticlustering, also considering skew and curtosis
anticlusters_skew_kurtosis <- anticlustering(
  scale(extended_skew_kurtosis),
  K = 3,
  objective = "variance"
)


by(features, anticlusters_skew_kurtosis, function(x) round(apply(x, 2, Skew), 2))
by(features, anticlusters_cov, function(x) round(apply(x, 2, Skew), 2))

by(features, anticlusters_skew_kurtosis, function(x) round(apply(x, 2, Kurt), 2))
by(features, anticlusters_cov, function(x) round(apply(x, 2, Kurt), 2))

mean_sd_tab(features, anticlusters_skew_kurtosis)
mean_sd_tab(features, anticlusters_cov)

