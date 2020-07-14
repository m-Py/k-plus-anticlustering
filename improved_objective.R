
library(anticlust)

N <- 150
M <- 2
K <- 3
features <- matrix(rnorm(N * M), ncol = M)
#features <- schaper2019[, 3:6]

nrep <- 30

ac_kmeans <- anticlustering(
  features,
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

ac_distance <- anticlustering(
  features,
  K = K,
  objective = "distance",
  method = "local-maximum",
  repetitions = nrep,
)

ac_kmeans_var_only <- anticlustering(
  squared_from_mean(features),
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

ac_kmeans_var <- anticlustering(
  cbind(features, squared_from_mean(features)),
  K = K,
  objective = "variance",
  method = "local-maximum",
  repetitions = nrep
)

cat("Standard K-Means \n")
print(mean_sd_tab(features, ac_kmeans, return_diff = TRUE))
cat("K-Means + Squared Diff\n")
print(mean_sd_tab(features, ac_kmeans_var, return_diff = TRUE))
#cat("Anticluster Editing \n")
#print(mean_sd_tab(features, ac_distance, return_diff = TRUE))
cat("Only Squared Diff\n")
print(mean_sd_tab(features, ac_kmeans_var_only, return_diff = TRUE))

variance_objective(squared_from_mean(features), ac_kmeans_var) / 
  variance_objective(squared_from_mean(features), ac_kmeans_var_only)

variance_objective(features, ac_kmeans_var) / variance_objective(features, ac_kmeans)


# WHY IS K-MEANS EXTENDED BETTER AT MINIMIZING MAXIMUM DIFFERENCE IN 
# VARIANCE THAN K-VARIANCE ALONE? NOT SURE YET. (and this is only the 
# case for K > 2)
sum(group_diff_min_max(ac_kmeans, features, var))
sum(group_diff_min_max(ac_kmeans_var, features, var))
sum(group_diff_min_max(ac_kmeans_var_only, features, var))

