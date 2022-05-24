
library(tidyr)
library(dplyr)
library(anticlust)

oasis <- read.csv(
  "https://raw.githubusercontent.com/aenneb/OASIS-beauty/master/means_per_image.csv"
)

features <- subset(oasis, select = c(beauty_mean, Valence_mean, Arousal_mean))

K <- 9

# k-plus variance / skew
start <- Sys.time()
groups_kplus <- kplus_anticlustering(
  features,
  K = K,
  variance = TRUE,
  skew = TRUE,
  method = "local-maximum",
  standardize = TRUE
)
Sys.time() - start

# k-means
start <- Sys.time()
groups_kmeans <- anticlustering(
  features,
  K = K,
  objective = "variance",
  method = "local-maximum",
  standardize = TRUE
)
Sys.time() - start

# diversity
start <- Sys.time()
groups_diversity <- anticlustering(
  features,
  K = K,
  objective = "diversity",
  method = "local-maximum",
  standardize = TRUE
)
Sys.time() - start


# Functions to compute means / SD / skew per group 
descriptives_by_group <- function(features, anticlusters, FUN, name) {
  df <- data.frame(
    t(data.frame(lapply(by(features, anticlusters, FUN), c)))
  )
  df$group <- 1:nrow(df)
  df$Descriptive = name
  df
}


all_descriptives_by_group <- function(features, anticlusters) {
  means <- descriptives_by_group(features, anticlusters, colMeans, "mean")
  sds <- descriptives_by_group(
    features, 
    anticlusters, 
    function(x) sapply(x, sd),
    "sds"
  )
  skew <- descriptives_by_group(
    features, 
    anticlusters, 
    function(x) sapply(x, DescTools::Skew),
    "skew"
  )
  rbind(means, sds, skew)
}


kplus <- all_descriptives_by_group(features, groups_kplus)
kplus <- data.frame(kplus, Objective = "kplus")

kmeans <- all_descriptives_by_group(features, groups_kmeans)
kmeans <- data.frame(kmeans, Objective = "kmeans")
diversity <- all_descriptives_by_group(features, groups_diversity)
diversity <- data.frame(diversity, Objective = "diversity")

all_objs <- rbind(kplus, kmeans, diversity) %>% 
  pivot_wider(names_from = Descriptive, values_from = c(beauty_mean, Valence_mean, Arousal_mean))

