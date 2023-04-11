
weight_selection_sim <- function(X, mean_distr = 0, sd_distr = 1) {
  N <- 14
  K <- 2
  M <- 5
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
  
  squared_distances_from_mean <- squared_from_mean(as.matrix(features))
  
  # 2. SSE(VAR)
  all_objectives_kVar <- sapply(
    partitions,
    FUN = var_objective,
    features = squared_distances_from_mean
  )
  
  # 3. SSE(KPLUS)
  all_objectives_kPlus <- sapply(
    partitions,
    FUN = var_objective,
    features = cbind(features, squared_distances_from_mean)
  )
  
  # 4. SSE(KPLUS) standardize
  # Use standardization to compute k-plus criterion!
  all_objectives_kPlus_std <- sapply(
    partitions,
    FUN = var_objective,
    features = scale(cbind(features, squared_distances_from_mean))
  )
  
  df <- data.frame(
    kmeans = all_objectives_kMeans,
    kvar = all_objectives_kVar,
    kplus = all_objectives_kPlus,
    kplus_std = all_objectives_kPlus_std,
    kplus_mean_prefered_5 = all_objectives_kMeans * 5 + all_objectives_kVar,
    kplus_var_prefered_5 = all_objectives_kMeans  + all_objectives_kVar * 5,
    kplus_mean_prefered_10 = all_objectives_kMeans * 10 + all_objectives_kVar,
    kplus_var_prefered_10 = all_objectives_kMeans  + all_objectives_kVar * 10
  )
  
  
  # how good is each k-plus solution
  # not standardized
  return(
    c(
      unstd_unweighted_mean = df$kmeans[which.max(df$kplus)] / max(df$kmeans),
      unstd_unweighted_var = df$kvar[which.max(df$kplus)] / max(df$kvar),
      std_unweighted_mean = df$kmeans[which.max(df$kplus_std)] / max(df$kmeans),
      std_unweighted_var = df$kvar[which.max(df$kplus_std)] / max(df$kvar),
      mean_preferred_5_mean = df$kmeans[which.max(df$kplus_mean_prefered_5)] / max(df$kmeans),
      mean_preferred_5_var = df$kvar[which.max(df$kplus_mean_prefered_5)] / max(df$kvar),
      var_preferred_5_mean = df$kmeans[which.max(df$kplus_var_prefered_5)] / max(df$kmeans),
      var_preferred_5_var = df$kvar[which.max(df$kplus_var_prefered_5)] / max(df$kvar),
      mean_preferred_10_mean = df$kmeans[which.max(df$kplus_mean_prefered_10)] / max(df$kmeans),
      mean_preferred_10_var = df$kvar[which.max(df$kplus_mean_prefered_10)] / max(df$kvar),
      var_preferred_10_mean = df$kmeans[which.max(df$kplus_var_prefered_10)] / max(df$kmeans),
      var_preferred_10_var = df$kvar[which.max(df$kplus_var_prefered_10)] / max(df$kvar)
    )
  )
}

tt <- sapply(1:10, weight_selection_sim)
(rowMeans(tt) * 100) |> round(2)
