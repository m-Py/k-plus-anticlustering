library(anticlust)

# Compute features for k-plus criterion
moment_features <- function(data, moment = 2) {
  apply(data, 2, function(x) (x - mean(x))^moment)
}

kplus_objective_weighted <- function(features, clusters, moments, weights) {
  objectives <- rep(NA, length(moments))
  for (t in moments) {
    if (t == 1) {
      objectives[t] <- variance_objective(features, clusters)
    } else {
      moment_variables <- moment_features(features, moments[t])
      objectives[t] <- variance_objective(moment_variables, clusters)
    }
  }
  sum(objectives * weights)
}

kplus_mean_preferred <- function(features, clusters) {
  kplus_objective_weighted(features, clusters, 1:4, c(10, 1, 1, 1))
}

kplus_var_preferred <- function(features, clusters) {
  kplus_objective_weighted(features, clusters, 1:4, c(1, 10, 1, 1))
}

kplus_skew_preferred <- function(features, clusters) {
  kplus_objective_weighted(features, clusters, 1:4, c(1, 1, 10, 1))
}

kplus_kurtosis_preferred <- function(features, clusters) {
  kplus_objective_weighted(features, clusters, 1:4, c(1, 1, 1, 10))
}

clusters_for_all_methods <- function(data, K) {
  all_results <- list()
  repetitions <- 5
  method <- "local-maximum"
  # unweighted k-plus anticlustering really is just k-means anticlustering
  all_results[["unweighted_unstd"]] <- anticlustering(
    cbind(data, moment_features(data, 2), moment_features(data, 3), moment_features(data, 4)),
    K = K,
    objective = "variance", # k-means
    method = method,
    repetitions = repetitions
  )
  all_results[["unweighted_std"]] <- anticlustering(
    scale(cbind(data, moment_features(data, 2), moment_features(data, 3), moment_features(data, 4))),
    K = K,
    objective = "variance",
    method = method,
    repetitions = repetitions
  )
  
  all_results[["mean_preferred"]] <- anticlustering(
    data,
    K = K,
    objective = kplus_mean_preferred,
    method = method,
    repetitions = repetitions
  )
  all_results[["var_preferred"]] <- anticlustering(
    data,
    K = K,
    objective = kplus_var_preferred,
    method = method,
    repetitions = repetitions
  )
  all_results[["skew_preferred"]] <- anticlustering(
    data,
    K = K,
    objective = kplus_skew_preferred,
    method = method,
    repetitions = repetitions
  )
  all_results[["kurtosis_preferred"]] <- anticlustering(
    data,
    K = K,
    objective = kplus_kurtosis_preferred,
    method = method,
    repetitions = repetitions
  )
  all_results
}


weight_sim <- function(dat, K) {
  clusters <- clusters_for_all_methods(dat, K)
  
  objective_results <- matrix(NA, ncol = 4, nrow = length(clusters))
  rownames(objective_results) <- names(clusters)
  for (i in 1:nrow(objective_results)) {
    for (moment in 1:4) {
      if (moment == 1) {
        objective_results[i, moment] <- variance_objective(dat, clusters[[i]])
      } else {
        objective_results[i, moment] <- variance_objective(moment_features(dat, moment), clusters[[i]])
      }
    }
  }
  
  # get relative performance: divide by best result in the simulation run, per criterion
  results <- t(t(objective_results) / apply(objective_results, 2, max))
  results 
}
