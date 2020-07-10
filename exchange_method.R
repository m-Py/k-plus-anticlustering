
# Call the exchange method below until a local maximum is reached
local_maximum_exchange <- function(features, init, obj_function) {
  best_partition <- init
  found_partitions <- list(best_partition)
  new_obj <- variance_objective(features, best_partition)
  old_obj <- -Inf
  while (new_obj > old_obj) {
    partitions <- exchange_method(
      features,
      clusters = best_partition, 
      obj_function = obj_function
    )
    best_partition <- partitions[[length(partitions)]]
    old_obj <- new_obj
    new_obj <- obj_function(features, best_partition)
    found_partitions <- c(found_partitions, partitions)
  }
  found_partitions[!duplicated(found_partitions)]
} 


#' Solve anticlustering using the modified exchange method
#'
#' @param data the data -- a N x N dissimilarity matrix or a N x M
#'     table of item features
#' @param init An initial assignment of clusters to groups (integer vector)
#' @param obj_function the objective function. Takes as first argument
#'     a cluster assignment and as second argument the data set `data`
#'     (`data` is matrix, no `data.frame`).
#'
#' @return The anticluster assignment
#'
#' @noRd
#'
#'

exchange_method <- function(data, clusters, obj_function) {

  all_partitions <- list(order_cluster_vector(clusters))
  u <- 2
  N <- nrow(data)
  best_total <- obj_function(data, clusters)
  for (i in 1:N) {
    # exchange partners to not have the same cluster
    feasible_partners <- (clusters != clusters[i])
    exchange_partners <- (1:N)[feasible_partners]

    # container to store objectives associated with each exchange of item i:
    comparison_objectives <- rep(NA, length(exchange_partners))
    for (j in seq_along(exchange_partners)) {
      ## Swap item i with all legal exchange partners and check out objective
      comparison_objectives[j] <- update_objective_generic(
        data,
        clusters,
        i,
        exchange_partners[j],
        obj_function
      )
    }
    ## Do the swap if an improvement occured
    best_this_round <- max(comparison_objectives)
    if (best_this_round > best_total) {
      # Which element has to be swapped
      swap <- exchange_partners[comparison_objectives == best_this_round][1]
      # Swap the elements
      clusters <- cluster_swap(clusters, i, swap)
      all_partitions[[u]] <- order_cluster_vector(clusters)
      u <- u + 1
      # Update best solution
      best_total <- best_this_round
    }
  }
  all_partitions
}

# Update the objective value - generic version taking any objective function
# param data: the data (distances/features)
# param clusters: a vector of clusters
# param i, j: the to be swapped items (indexes)
# param obj_function: The function that computes the objective value
# return: The objective after a hypothetical swap
update_objective_generic <- function(data, clusters, i, j, obj_function) {
  tmp_clusters <- cluster_swap(clusters, i, j)
  obj_function(data, tmp_clusters)
}

# Swap two items and return new arranged clusters
#
# param clusters: a vector of clusters
# param i, j: the to be swapped items (indexes of the items)
# return: the cluster vector after swapping items i and j
cluster_swap <- function(clusters, i, j) {
  group_i <- clusters[i]
  clusters[i] <- clusters[j]
  clusters[j] <- group_i
  clusters
}

# Order a clustering vector
# 
# For a clustering vector, ensure that the first cluster that occurs
# in the vector is 1, the next 2, etc ... until K
#
# param clusters: A clustering vector with elements 1, ..., K indicating cluster membership
# return: A clustering vector in order
order_cluster_vector <- function(clusters) {
  unique_clusters <- unique(clusters)
  # deal with NA
  unique_clusters <- unique_clusters[!is.na(unique_clusters)]
  K <- length(unique_clusters)
  clusters <- factor(clusters, levels = unique_clusters, labels = 1:K)
  as.numeric(clusters)
}
