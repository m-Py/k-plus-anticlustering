
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

# Reason: I validated the results using difference between minimum and
# maximum *standard deviation* across groups. For K > 2, this is not 
# equivalent to minimum and maximum *variance* across groups (which
# is however minimized using the k-variance criterion!)

library(anticlust)
source("exchange_method.R")
source("../misc-functions.R")

N <- 200
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

objectives <- list()
objectives[["diff_mean"]] <- list()
objectives[["diff_var"]] <- list()

funs <- list(diff_mean = mean, diff_var = var)

for (i in c("diff_mean", "diff_var")) {
  objectives[[i]][["kMeans"]] <- sapply(
    partitions_means,
    sum_group_diff_min_max,
    features = features,
    fun = funs[[i]]
  )
  objectives[[i]][["kVar"]] <- sapply(
    partitions_variance,
    sum_group_diff_min_max,
    features = features,
    fun = funs[[i]]
  )
  objectives[[i]][["kExt"]] <- sapply(
    partitions_means_variance,
    sum_group_diff_min_max,
    features = features,
    fun = funs[[i]]
  )
}



sample_point <- function(clusters, features) {
  cl <- sample(clusters)
  c(
    mean = sum_group_diff_min_max(cl, features, fun = mean),
    var = sum_group_diff_min_max(cl, features, fun = var)
  )
}

pts <- data.frame(t(replicate(1000, sample_point(init, features))))
plot(
  pts, 
  col = "darkgrey", 
  las = 1, 
  pch = 4, 
  cex = 0.8,
  xlim = c(0, max(pts$mean)),
  ylim = c(0, max(pts$var)),
  xlab = "Diff Min/Max Mean",
  ylab = "Diff Min/Max Variance"
)

points(
  objectives[["diff_mean"]][["kMeans"]],
  objectives[["diff_var"]][["kMeans"]],
  col = "#28E2E5",
  pch = 19, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  objectives[["diff_mean"]][["kVar"]],
  objectives[["diff_var"]][["kVar"]],
  col = "#2297E6",
  pch = 18, 
  cex = 2,
  type = "b",
  lwd = 2
)

points(
  objectives[["diff_mean"]][["kExt"]],
  objectives[["diff_var"]][["kExt"]],
  col = "#CD0BBC",
  pch = 17, 
  cex = 2,
  type = "b",
  lwd = 2
)
points(objectives[["diff_mean"]][[c(1, 1)]], 
       objectives[["diff_var"]][[c(1, 1)]],
       cex = 3, pch = 15)

legend("topright", legend = c("k-means", "k-variance", "k-extended"),
       pch = c(19, 18, 17), col = c("#28E2E5", "#2297E6", "#CD0BBC"))

sum_group_diff_min_max(last(partitions_means), features, var)
sum_group_diff_min_max(last(partitions_variance), features, var)
sum_group_diff_min_max(last(partitions_means_variance), features, var)

# randomly sample points to illustrate the solution space
