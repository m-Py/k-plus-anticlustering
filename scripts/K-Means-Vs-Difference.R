
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

# Reason: I validated the results using difference between minimum and
# maximum *standard deviation* across groups. For K > 2, this is not 
# equivalent to minimum and maximum *variance* across groups (which
# is however minimized using the k-variance criterion!)

library(anticlust)
source("exchange_method.R")
source("../misc-functions.R")

N <- 15
M <- 3
K <- 3
features <- matrix(rnorm(N * M), ncol = M)
all_partitions <- generate_partitions(N, K)

all_objectives_var <- sapply(
  all_partitions,
  FUN = sum_group_diff_min_max,
  features = features,
  fun = var
)

all_objectives_mean <- sapply(
  all_partitions,
  FUN = sum_group_diff_min_max,
  features = features,
  fun = mean
)

all_objectives_kVariances <- sapply(
  all_partitions,
  FUN = var_objective,
  features = squared_from_mean(features)
)

all_objectives_kMeans <- sapply(
  all_partitions,
  FUN = var_objective,
  features = features
)

all_objectives_kExtended <- sapply(
  all_partitions,
  FUN = var_objective,
  features = cbind(squared_from_mean(features), features)
)

df <- data.frame(
  kVar = all_objectives_kVariances,
  diff_var  = all_objectives_var,
  kMeans = all_objectives_kMeans,
  diff_means  = all_objectives_mean,
  kExt = all_objectives_kExtended
)

#plot(df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)


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
points(plot_df[match(list(init), all_partitions), , drop = FALSE], cex = 3, pch = 15)

legend("topright", legend = c("k-means", "k-variance", "k-extended"),
       pch = c(19, 18, 17), col = c("#28E2E5", "#2297E6", "#CD0BBC"))

# Difference between min/max variance or SD gives different results 
# with regard to similarity of the groups!
fun <- var

variance_objective(squared_from_mean(features), last(partitions_means_variance))
variance_objective(squared_from_mean(features), last(partitions_variance))

sum_group_diff_min_max(last(partitions_means_variance), features, fun)
sum_group_diff_min_max(last(partitions_variance), features, fun)

round(cor(plot_df), 2)
