
library(anticlust)

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

improved_exchange_ <- function(nrep, x, K, objective, obj_function, 
                               categories, preclustering) {
  x <- as.matrix(x)
  N <- nrow(x)
  clusters <- anticlustering(
    x, K, objective = objective, categories = categories
  )
  new_obj <- obj_function(x, clusters)
  old_obj <- -1
  while (new_obj > old_obj) {
    clusters <- anticlustering(
      x, K, objective = objective
    )
    old_obj <- new_obj
    new_obj <- obj_function(x, clusters)
  }
  clusters
}

improved_exchange <- function(x, K, objective, obj_function, 
                              categories = NULL, preclustering = FALSE, 
                              nrep = 10) {
  candidate_solutions <- lapply(
    1:nrep, 
    improved_exchange_,
    x = x, 
    K = K, 
    objective = objective, 
    obj_function = obj_function,
    categories = categories, 
    preclustering = preclustering
  )
  obj_function_ <- function(clusters, x) {
    obj_function(x, clusters)
  }
  objs <- lapply(
    candidate_solutions,
    obj_function_,
    x = x
  )
  candidate_solutions[[which.max(objs)]]
}


N <- 500
M <- 2
K <- 10 
features <- matrix(rchisq(N * M, 2), ncol = M)
#features <- scale(schaper2019[, 3:6])

nrep <- 20
preclustering <- FALSE

ac_kmeans <- improved_exchange(
  features,
  K = K,
  objective = "variance",
  obj_function = variance_objective, 
  preclustering = preclustering,
  nrep = nrep
)

ac_distance <- improved_exchange(
  features,
  K = K,
  objective = "distance",
  obj_function = variance_objective, 
  preclustering = preclustering,
  nrep = nrep
)

ac_kmeans_var_only <- improved_exchange(
  squared_from_mean(features),
  K = K,
  objective = "variance",
  obj_function = variance_objective,
  preclustering = preclustering,
  nrep = nrep
)

ac_kmeans_var <- improved_exchange(
  cbind(features, squared_from_mean(features)),
  K = K,
  objective = "variance",
  obj_function = variance_objective,
  preclustering = preclustering,
  nrep = nrep
)

cat("Standard K-Means \n")
print(mean_sd_tab(features, ac_kmeans, return_diff = TRUE))
cat("K-Means + Squared Diff\n")
print(mean_sd_tab(features, ac_kmeans_var, return_diff = TRUE))
cat("Anticluster Editing \n")
print(mean_sd_tab(features, ac_distance, return_diff = TRUE))
#cat("Only Squared Diff\n")
#print(mean_sd_tab(features, ac_kmeans_var_only, return_diff = TRUE))



