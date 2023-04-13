library(anticlust)

var_objective <- function(clusters, features) {
  variance_objective(features, clusters)
}

# Compute features for k-plus criterion
moment_features <- function(data, moment = 2) {
  apply(data, 2, function(x) (x - mean(x))^moment)
}

weight_selection_sim <- function(X, mean_distr = 0, sd_distr = 1) {
  N <- sample(c(12, 14, 16), size = 1)
  K <- 2
  M <- sample(1:5, size = 1)
  partitions <- generate_partitions(N, K)
  features <- matrix(rnorm(N * M, mean_distr, sd_distr), ncol = M)
  
  # Compute objectives for each partition
  # For each partition: Compute SSE objective, and SSE(VAR)
  # 1. SSE
  all_objectives_kMeans <- sapply(
    partitions,
    FUN = var_objective,
    features = features
  )
  
  squared_distances_from_mean <- moment_features(as.matrix(features))
  cubic_distances_from_mean <- moment_features(as.matrix(features), moment = 3)
  
  # 2. SSE(VAR)
  all_objectives_kVar <- sapply(
    partitions,
    FUN = var_objective,
    features = squared_distances_from_mean
  )
  
  # 3. SSE(skew)
  all_objectives_skew <- sapply(
    partitions,
    FUN = var_objective,
    features = cubic_distances_from_mean
  )
  
  # 4. SSE(KPLUS)
  all_objectives_kPlus <- sapply(
    partitions,
    FUN = var_objective,
    features = cbind(features, squared_distances_from_mean, cubic_distances_from_mean)
  )
  
  # 5. SSE(KPLUS) - standardized
  # Use standardization to compute k-plus criterion!
  all_objectives_kPlus_std <- sapply(
    partitions,
    FUN = var_objective,
    features = scale(cbind(features, squared_distances_from_mean, cubic_distances_from_mean))
  )
  
  df <- data.frame(
    kmeans = all_objectives_kMeans,
    kvar = all_objectives_kVar,
    kskew = all_objectives_skew,
    kplus = all_objectives_kPlus,
    kplus_std = all_objectives_kPlus_std,
    kplus_mean_prefered = all_objectives_kMeans * 10 + all_objectives_kVar + all_objectives_skew,
    kplus_var_prefered = all_objectives_kMeans + all_objectives_kVar * 10 + all_objectives_skew,
    kplus_skew_prefered = all_objectives_kMeans + all_objectives_kVar + all_objectives_skew * 10
  )
  
  
  # how good is each k-plus solution
  # not standardized
  return(
    c(
      unstd_unweighted_mean = df$kmeans[which.max(df$kplus)] / max(df$kmeans),
      unstd_unweighted_var = df$kvar[which.max(df$kplus)] / max(df$kvar),
      unstd_unweighted_skew = df$kskew[which.max(df$kplus)] / max(df$kskew),
      std_unweighted_mean = df$kmeans[which.max(df$kplus_std)] / max(df$kmeans),
      std_unweighted_var = df$kvar[which.max(df$kplus_std)] / max(df$kvar),
      std_unweighted_skew = df$kskew[which.max(df$kplus_std)] / max(df$kskew),
      mean_preferred_mean = df$kmeans[which.max(df$kplus_mean_prefered)] / max(df$kmeans),
      mean_preferred_var = df$kvar[which.max(df$kplus_mean_prefered)] / max(df$kvar),
      mean_preferred_skew = df$kskew[which.max(df$kplus_mean_prefered)] / max(df$kskew),
      var_preferred_mean = df$kmeans[which.max(df$kplus_var_prefered)] / max(df$kmeans),
      var_preferred_var = df$kvar[which.max(df$kplus_var_prefered)] / max(df$kvar),
      var_preferred_skew = df$kskew[which.max(df$kplus_var_prefered)] / max(df$kskew),
      skew_preferred_mean = df$kmeans[which.max(df$kplus_skew_prefered)] / max(df$kmeans),
      skew_preferred_var = df$kvar[which.max(df$kplus_skew_prefered)] / max(df$kvar),
      skew_preferred_skew = df$kskew[which.max(df$kplus_skew_prefered)] / max(df$kskew)
    )
  )
}

tt <- sapply(1:15, weight_selection_sim)

mat <- matrix(
  rowMeans(tt) * 100, 
  ncol = 3, byrow = TRUE, 
  dimnames = list(
    c("unstd_unweighted", "std_unweighted", "mean_prefered", "var_prefered", "skew_prefered"), 
    c("kmeans", "kvar", "kskew")
  )
)
mat |> round(2)
rowMeans(mat) |> round(2)
