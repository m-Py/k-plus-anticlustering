
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

source("exchange_method.R")
source("misc-functions.R")


library(anticlust)
library(tidyverse)

N <- 100
M <- 4
K <- 4
features <- matrix(rnorm(N * M), ncol = M)
dist_features <- squared_from_mean(features)
combined_features <- cbind(features, dist_features)

# Initialize a partition
init <- sample(rep_len(1:K, N))
init <- order_cluster_vector(init)

# Do local search for three objectives
partitions_means <- local_maximum_exchange(
  features, 
  init
)
partitions_variance <- local_maximum_exchange(
  dist_features, 
  init
)
partitions_means_variance <- local_maximum_exchange(
  combined_features, 
  init
)

partitions <- list(
  kMeans = partitions_means, 
  kVariance = partitions_variance, 
  kExtended = partitions_means_variance
)
all_features <- list(
  kMeans = features, 
  kVariance = dist_features
)

results_list <- list()
# iterate over data and objectives
for (i in names(partitions)) {
  iteration <- 1:length(partitions[[i]])
  for (j in names(all_features)) {
    objectives <- sapply(partitions[[i]], var_objective, data = all_features[[j]])
    results_list[[paste0(i, "-", j)]] <- data.frame(
      objectives, method = i, objective = j, iteration = iteration
    )
  }
}
results <- do.call(rbind, results_list)
rownames(results) <- NULL

results <- pivot_wider(results, names_from = objective, values_from = objectives)


# Plot data
ggplot(results, aes(x = kMeans, y = kVariance, group = method)) +
  geom_path(aes(col = method)) + 
  geom_point(aes(col = method)) + 
  geom_point(aes(x = kMeans[1], y = kVariance[1]))

options(pillar.sigfig = 6)

results %>% 
  group_by(method) %>% 
  summarize(
    kVarianceObj = max(kVariance),
    kMeansObj = max(kMeans))

best_mean <- partitions_means[[length(partitions_means)]]
best_var <- partitions_variance[[length(partitions_variance)]]
best_mean_var <- partitions_means_variance[[length(partitions_means_variance)]]

sum(group_diff_min_max(best_mean, features, var))
sum(group_diff_min_max(best_var, features, var))
sum(group_diff_min_max(best_mean_var, features, var))
