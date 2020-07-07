
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

library(anticlust)

N <- 16
K <- 2
all_partitions <- generate_partitions(N, K)
features <- as.matrix(rnorm(N))

## Create an objective function that takes the partition
## as first argument (then, we can use sapply to compute
## the objective for each partition)
diff_mean <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, mean))))
}

diff_vars <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, var))))
}

all_objectives_mean <- sapply(
  all_partitions,
  FUN = diff_mean,
  features = features
)

all_objectives_var <- sapply(
  all_partitions,
  FUN = diff_vars,
  features = features
)

df <- data.frame(
  diff_mean = all_objectives_mean,
  diff_var  = all_objectives_var
)

# exchange method for finding all objectives

local_maximum_exchange <- function(features, init) {
  best_partition <- init
  found_partitions <- list(best_partition)
  new_obj <- variance_objective(features, best_partition)
  old_obj <- -Inf
  while (new_obj > old_obj) {
    partitions <- anticlustering(
      features,
      K = best_partition, 
      objective = variance_objective
    )
    best_partition <- partitions[[length(partitions)]]
    old_obj <- new_obj
    new_obj <- variance_objective(features, best_partition)
    found_partitions <- c(found_partitions, partitions)
  }
  found_partitions[!duplicated(found_partitions)]
} 

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

# Initialize a partition
init <- sample(rep_len(1:K, N))
init <- anticlust:::order_cluster_vector(init)

# Do local search for three objectives
partitions_means <- local_maximum_exchange(
  features, 
  init
)
partitions_variance <- local_maximum_exchange(
  squared_from_mean(features), 
  init
)
partitions_means_variance <- local_maximum_exchange(
  cbind(features, squared_from_mean(features)), 
  init
)

plot(df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)
points(
  df[match(partitions_means, all_partitions), ], 
  col = "#28E2E5",
  pch = 19, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  df[match(partitions_variance, all_partitions), ], 
  col = "#2297E6",
  pch = 18, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  df[match(partitions_means_variance, all_partitions), ], 
  col = "#CD0BBC",
  pch = 17, 
  cex = 2,
  type = "b",
  lwd = 2
)
points(df[match(list(init), all_partitions), , drop = FALSE], cex = 2, pch = 19)
