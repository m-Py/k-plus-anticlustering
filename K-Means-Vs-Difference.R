
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

# Reason: I validated the results using difference between minimum and
# maximum *standard deviation* across groups. For K > 2, this is not 
# equivalent to minimum and maximum *variance* across groups (which
# is however minimized using the k-variance criterion!)

library(anticlust)
source("exchange_method.R")

N <- 12
K <- 3
all_partitions <- generate_partitions(N, K)
features <- as.matrix(rnorm(N))

## Create an objective function that takes the partition
## as first argument (then, we can use sapply to compute
## the objective for each partition)

sum_diff_vars <- function(clusters, features) {
  features <- as.matrix(features) 
  sum(apply(features, 2, diff_vars, features = features, clusters = clusters))
}

diff_vars <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, var))))
}

diff_mean <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, mean))))
}

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

obj_fun <- function(clusters, features) {
  variance_objective(features, clusters)
}

all_objectives_var <- sapply(
  all_partitions,
  FUN = diff_vars,
  features = features
)

all_objectives_mean <- sapply(
  all_partitions,
  FUN = diff_mean,
  features = features
)

all_objectives_kVariances <- sapply(
  all_partitions,
  FUN = obj_fun,
  features = squared_from_mean(features)
)

all_objectives_kMeans<- sapply(
  all_partitions,
  FUN = obj_fun,
  features = features
)

all_objectives_kExtended <- sapply(
  all_partitions,
  FUN = obj_fun,
  features = cbind(squared_from_mean(features), features)
)

df <- data.frame(
  kVar = all_objectives_kVariances,
  diff_var  = all_objectives_var,
  kMeans = all_objectives_kMeans,
  diff_means  = all_objectives_mean,
  kExt = all_objectives_kExtended
)

plot(
  df, 
  col = "darkgrey", 
  las = 1, 
  pch = 4, 
  cex = 0.8
)


# Initialize a partition
init <- sample(rep_len(1:K, N))
init <- anticlust:::order_cluster_vector(init)

# Do local search for three objectives
partitions_means <- local_maximum_exchange(
  features, 
  init,
  anticlust::variance_objective
)
partitions_variance <- local_maximum_exchange(
  squared_from_mean(features), 
  init,
  anticlust::variance_objective
)
partitions_means_variance <- local_maximum_exchange(
  cbind(features, squared_from_mean(features)), 
  init,
  anticlust::variance_objective
)

plot_df <- df[, c("diff_var", "diff_means")]

plot(plot_df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)
points(
  plot_df[match(partitions_means, all_partitions), ], 
  col = "#28E2E5",
  pch = 19, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  plot_df[match(partitions_variance, all_partitions), ], 
  col = "#2297E6",
  pch = 18, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  plot_df[match(partitions_means_variance, all_partitions), ], 
  col = "#CD0BBC",
  pch = 17, 
  cex = 2,
  type = "b",
  lwd = 2
)
points(plot_df [match(list(init), all_partitions), , drop = FALSE], cex = 2, pch = 19)

legend("topright", legend = c("k-means", "k-variance", "k-extended"),
       pch = c(19, 18, 17), col = c("#28E2E5", "#2297E6", "#CD0BBC"))

nth <- function(x) {
  x[[length(x)]]
}

# Check out similarity of item means (rounded on two decimals)
fun_by_group <- function(features, anticlusters, fun) {
  data.frame(lapply(by(features, anticlusters, function(x) apply(x, 2, fun)), c))
}


# Difference between min/max variance or SD gives different results 
# with regard to similarity of the groups!
fun <- var

diff(range(fun_by_group(features, nth(partitions_means), fun)))
diff(range(fun_by_group(features, nth(partitions_means_variance), fun)))
diff(range(fun_by_group(features, nth(partitions_variance), fun)))
