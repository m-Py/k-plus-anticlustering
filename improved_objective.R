
library(anticlust)

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}


N <- 60
M <- 1
K <- 3
features <- matrix(rnorm(N * M), ncol = M)
#features <- schaper2019[, 3:6]

nrep <- 30

ac_kmeans <- anticlustering(
  features,
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

ac_distance <- anticlustering(
  features,
  K = K,
  objective = "distance",
  method = "local-maximum",
  repetitions = nrep,
)

ac_kmeans_var_only <- anticlustering(
  squared_from_mean(features),
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

ac_kmeans_var <- anticlustering(
  scale(cbind(features, squared_from_mean(features))),
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

cat("Standard K-Means \n")
print(mean_sd_tab(features, ac_kmeans, return_diff = TRUE))
cat("K-Means + Squared Diff\n")
print(mean_sd_tab(features, ac_kmeans_var, return_diff = TRUE))
cat("Anticluster Editing \n")
print(mean_sd_tab(features, ac_distance, return_diff = TRUE))
cat("Only Squared Diff\n")
print(mean_sd_tab(features, ac_kmeans_var_only, return_diff = TRUE))
