
options(scipen = 999)

library(anticlust)

# Determine the maximum range in a vector
range_diff <- function(x) {
  diff(range(x))
}

compare_objectives <- function(i, N = 100, K = 2, preclustering = FALSE) {
  cat("Simulation run", i, "\n")
  data <- rnorm(N)
  data_var <- abs(data - mean(data))
  ac <- anticlustering(
    data, 
    K = K,
    objective = "variance"
  )
  var_only <- anticlustering(
    data_var, 
    K = K,
    objective = "variance"
  )
  ac_var <- anticlustering(
    cbind(data, data_var), 
    K = K,
    objective = "variance"
  )
  ac_var_pre <- anticlustering(
    cbind(data, data_var), 
    K = K,
    objective = "variance",
    preclustering = TRUE
  )
  ac_dist <- anticlustering(
    data, 
    K = K,
    objective = "distance",
  )
  ac_dist_pre <- anticlustering(
    data, 
    K = K,
    objective = "distance",
    preclustering = TRUE
  )
  
  random <- sample(ac_dist)
  
  list(
    DIFF_MEANS = c(
      kmeans = range_diff(tapply(data, ac, mean)),
      kmeansVAR = range_diff(tapply(data, ac_var, mean)),
      kmeansVAR_PRE = range_diff(tapply(data, ac_var_pre, mean)),
      kmeansVARONLY = range_diff(tapply(data, var_only, mean)),
      MDGP = range_diff(tapply(data, ac_dist, mean)),
      MDGP_PRE = range_diff(tapply(data, ac_dist_pre, mean)),
      RANDOM = range_diff(tapply(data, random, mean))
    ),
    DIFF_VARS = c(
      kmeans = range_diff(tapply(data, ac, var)),
      kmeansVAR = range_diff(tapply(data, ac_var, var)),
      kmeansVAR_PRE = range_diff(tapply(data, ac_var_pre, var)),
      kmeansVARONLY = range_diff(tapply(data, var_only, var)),
      MDGP = range_diff(tapply(data, ac_dist, var)),
      MDGP_PRE = range_diff(tapply(data, ac_dist_pre, var)),
      RANDOM = range_diff(tapply(data, random, var))
    )
  )
}

# Do simulation
nrep <- 50
N <- 300
K <- 2
comparison <- lapply(
  1:nrep, 
  FUN = compare_objectives, 
  N = N, 
  K = K
)

# Read difference in means and variances, respectively, from results
means <- simplify2array(lapply(comparison, function(x) x[["DIFF_MEANS"]]))
vars <- simplify2array(lapply(comparison, function(x) x[["DIFF_VARS"]]))

# Illustrate aggregated results
print(round(data.frame(
  diff_means = rowMeans(means),
  diff_vars = rowMeans(vars)
), 4))



