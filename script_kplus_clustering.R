# Compute features for k-plus criterion

library(anticlust)
moment_features <- function(data, moment = 2) {
  apply(data, 2, function(x) (x - mean(x))^moment)
}

kmeans_clustering_objective <- function(features, clusters) {
  -variance_objective(features, clusters)
}

kplus_clustering_objective <- function(features, clusters) {
  variance_vars <- moment_features(features, 2)
  all_vars <- scale(cbind(variance_vars))
  -variance_objective(all_vars, clusters)
}

# Load data
data(iris)
cluster_data <- iris[, 1:2]

# Do k-means clustering, store the clustering vector
clusters <- anticlustering(
  cluster_data,
  K = 3,
  objective = kmeans_clustering_objective,
  method = "local-maximum"
)

# define 3 colors, 3 cex values, and 3 pch values that differ between
# clusters - for more clusters, define more values.
colors <- c("#a9a9a9", "#df536b", "#61d04f")
cex <-  c(0.7, 1.2, 1.5)
pch <- c(19, 15, 17)

# Plot the data while visualizing the different clusters
plot(
  cluster_data,
  col = colors[clusters],
  cex = cex[clusters],
  pch = pch[clusters]
)

# Do k-means clustering, store the clustering vector
kplus_clusters <- anticlustering(
  cluster_data,
  K = 3,
  objective = kplus_clustering_objective,
  method = "local-maximum"
)

# Plot the data while visualizing the different clusters
plot(
  cluster_data,
  col = colors[kplus_clusters],
  cex = cex[kplus_clusters],
  pch = pch[kplus_clusters]
)



###################

library(anticlust)
library(fossil)
set.seed(12345)

g1 <- anticlustering(iris[, -5], K = 3, objective = "kplus")
g2 <- anticlustering(iris[, -5], K = 3, objective = "kplus")

# Compare the two anticlustering solutions:
rand.index(g1, g2)
# Compare to random grouping:
rand.index(sample(g1), sample(g2))

# Compare to two clustering solutions: 
c1 <- kmeans(iris[, -5], 3)$cluster
c2 <- balanced_clustering(iris[, -5], 3)

# High agreement, even though c2 is based on the restriction of equal group sizes:
rand.index(c1, c2)
