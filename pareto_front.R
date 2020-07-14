
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

library(anticlust)
source("misc-functions.R")

N <- 14
K <- 2
M <- 2
all_partitions <- generate_partitions(N, K)
features <- matrix(rnorm(N * M), ncol = M)

obj_fun <- function(partitions, features) {
  variance_objective(features, partitions)
}

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

all_objectives_mean <- sapply(
  all_partitions,
  FUN = obj_fun,
  features = features
)

all_objectives_var <- sapply(
  all_partitions,
  FUN = obj_fun,
  features = squared_from_mean(features)
)

df <- data.frame(
  kMeans = all_objectives_mean,
  kVariance  = all_objectives_var
)

# Compute Pareto efficient set!

plot(df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)

# For a data frame (rows = partitions, columns = objectives), determine which 
# elements are part of the Pareto efficient set. Output is a logical vector,
# illustrating for each row if it is part of the Pareto efficient set.
pareto_set <- function(df) {
  is_dominated <- rep(FALSE, nrow(df)) 
  for (i in 1:nrow(df)) {
    for (j in 1:nrow(df)) {
      j_dominates_i <- all(df[i, ] <= df[j, ]) && any(df[i, ] < df[j, ])
      if (j_dominates_i) {
        is_dominated[i] <- TRUE
        break
      }
    }
  }
  !is_dominated
}

efficient_set <- pareto_set(df)
plot(df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)
points(df[efficient_set, ], col = "red", cex = 1.2, pch = 19)


