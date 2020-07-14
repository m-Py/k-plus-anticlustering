
# Investigate why k-variance is worse than k-means-variance for 
# optimizing between-group similarity in variance

library(anticlust)
source("misc-functions.R")

N <- 12
K <- 3
M <- 2
all_partitions <- generate_partitions(N, K)
features <- matrix(rnorm(N * M), ncol = M)

all_objectives_mean <- sapply(
  all_partitions,
  FUN = var_objective,
  features = features
)

all_objectives_var <- sapply(
  all_partitions,
  FUN = var_objective,
  features = squared_from_mean(features)
)

df <- data.frame(
  kMeans = all_objectives_mean,
  kVariance  = all_objectives_var
)

# Compute Pareto efficient set!

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

# Try some speed optimization
pareto_set2 <- function(df) {
  df <- data.frame(df)
  ncrits <- ncol(df)
  df$ID <- 1:nrow(df)
  df <- df[order(rowSums(df[, 1:ncrits]), decreasing = TRUE), ]
  
  is_dominated <- rep(FALSE, nrow(df)) 
  for (i in nrow(df):1) {
    for (j in 1:nrow(df)) {
      j_dominates_i <- all(df[i, 1:ncrits] <= df[j, 1:ncrits]) && 
                       any(df[i, 1:ncrits] < df[j, 1:ncrits])
      if (j_dominates_i) {
        is_dominated[df$ID[i]] <- TRUE
        break
      }
    }
  }
  !is_dominated
}

#start <- Sys.time()
#efficient_set2 <- pareto_set2(df)
#Sys.time() - start 

#start <- Sys.time()
#efficient_set <- pareto_set(df)
#Sys.time() - start 
#all.equal(efficient_set, efficient_set2)


start <- Sys.time()
efficient_set <- pareto_set2(df)
Sys.time() - start 

sum(efficient_set)

plot(df, col = "darkgrey", las = 1, pch = 4, cex = 0.8)
points(df[efficient_set, ], col = "red", cex = 1.2, pch = 19)



# Do plot for difference in means / vars

df2 <- data.frame(
  diff_mean = sapply(
      all_partitions,
      sum_group_diff_min_max,
      features = features,
      fun = mean
  ),
  diff_var = sapply(
      all_partitions,
      sum_group_diff_min_max,
      features = features,
      fun = var
  )
)

plot(df2, col = "darkgrey", las = 1, pch = 4, cex = 0.8)
points(df2[efficient_set, ], col = "red", cex = 1.2, pch = 19)


