
# Call the exchange method below until a local maximum is reached
local_maximum_exchange <- function(features, init) {
  best_partition <- init
  found_partitions <- list(best_partition)
  new_obj <- variance_objective(features, best_partition)
  old_obj <- -Inf
  while (new_obj > old_obj) {
    partitions <- exchange_method(
      features,
      clusters = best_partition
    )
    best_partition <- partitions[[length(partitions)]]
    old_obj <- new_obj
    new_obj <- anticlust::variance_objective(features, best_partition)
    found_partitions <- c(found_partitions, partitions)
  }
  found_partitions[!duplicated(found_partitions)]
} 


#' Solve k-means anticlustering using the modified exchange method
#'
#' @param data the data -- an N x M table of item features
#' @param clusters An initial cluster assignment
#'
#' @return The anticluster assignment
#'
#' @noRd
#'

exchange_method <- function(data, clusters) {
  N <- nrow(data)
  best_total <- variance_objective_(clusters, data)
  centers <- cluster_centers(data, clusters)
  distances <- dist_from_centers(data, centers, squared = TRUE)
  all_partitions <- list(clusters)
  u <- 2
  
  ## frequencies of each cluster are required for updating cluster centers:
  tab <- c(table(clusters))
  for (i in 1:N) {
    # cluster of current item
    cluster_i <- clusters[i]
    # get exchange partners for item i
    exchange_partners <- 1:N
    # exchange partners are not in the same cluster:
    exchange_partners <- exchange_partners[clusters != clusters[i]]
    # Sometimes an exchange cannot take place
    if (length(exchange_partners) == 0) {
      next
    }
    # container to store objectives associated with each exchange of item i:
    comparison_objectives <- rep(NA, length(exchange_partners))
    for (j in seq_along(exchange_partners)) {
      ## Swap item i with all legal exchange partners and check out objective
      # (a) Determine clusters of to-be-swapped elements
      tmp_clusters <- clusters
      tmp_swap <- exchange_partners[j]
      cluster_j <- tmp_clusters[tmp_swap]
      # (b) Swap the elements
      tmp_clusters[i] <- cluster_j
      tmp_clusters[tmp_swap] <- cluster_i
      # (c) Update cluster centers after swap
      tmp_centers <- update_centers(centers, data, i, tmp_swap, cluster_i, cluster_j, tab)
      # (d) Update distances from centers after swap
      tmp_distances <- update_distances(data, tmp_centers, distances, cluster_i, cluster_j)
      # (e) Compute objective after swap
      comparison_objectives[j] <- sum(tmp_distances[cbind(1:nrow(tmp_distances), tmp_clusters)])
    }
    ## If an improvement of the objective occured, do the swap
    best_this_round <- max(comparison_objectives)
    if (best_this_round > best_total) {
      # which element has to be swapped
      swap <- exchange_partners[comparison_objectives == best_this_round][1]
      # Update cluster centers
      centers <- update_centers(centers, data, i, swap, clusters[i], clusters[swap], tab)
      # Update distances
      distances <- update_distances(data, centers, distances, cluster_i, clusters[swap])
      # Actually swap the elements - i.e., update clusters
      clusters[i] <- clusters[swap]
      clusters[swap] <- cluster_i
      all_partitions[[u]] <- clusters
      u <- u + 1
      # Update best solution
      best_total <- best_this_round
    }
  }
  all_partitions
}

# Internal function - no input handling
variance_objective_ <- function(clusters, data) {
  ## 1. Compute cluster centers
  centers <- cluster_centers(data, clusters)
  ## 2. For each item, compute distance to each cluster center
  distances <- dist_from_centers(data, centers, squared = TRUE)
  ## 3. Use two-column matrix to select relevant distances
  distances <- distances[cbind(1:nrow(distances), clusters)]
  sum(distances)
}


# Compute cluster centers
#
# @param features A data matrix of element features
# @param clusters A numeric vector indicating cluster membership of
#     each element
#
# @return A matrix of cluster centers. Rows represent clusters and
#   columns represent features
#

cluster_centers <- function(features, clusters) {
  centers <- by(features, clusters, colMeans, na.rm = TRUE)
  do.call(rbind, as.list(centers)) # as.list for the case of only one feature
}

# Determine distances of n data points to m cluster centers
#
#
# @param features A vector, matrix or data.frame of data points. If a
#     matrix or data.frame is passed, rows correspond to items and
#     columns to features.
# @param centers A matrix of cluster centers. Each row corresponds to a
#     cluster and each column corresponds to a feature (this format is,
#     for example, returned by the function `stats::kmeans` through the
#     element `centers`).
# @param squared Boolean - compute the squared euclidean distance?
#
# @return A data matrix; columns represent clusters
#     and contain the distance to the respective cluster for each item.
#
# @details
# This code was published in Leisch (2006).
#
# @references
# Leisch (2006). A Toolbox for K-Centroids Cluster Analysis. Computational
# Statistics and Data Analysis, 51(2), 526–544.
#
dist_from_centers <- function(features, centers, squared) {
  z <- matrix(0, nrow = nrow(features), ncol = nrow(centers))
  for (k in 1:nrow(centers)) {
    if (squared)
      z[,k] <- colSums((t(features) - centers[k,])^2, na.rm = TRUE)
    else
      z[,k] <- sqrt( colSums((t(features) - centers[k,])^2, na.rm = TRUE) )
  }
  z
}


#' Recompute distances from cluster centers after swapping two elements
#' @param distances distances from cluster centers per element (old)
#' @param cluster_i the cluster of element i
#' @param cluster_j the cluster of element j
#' @return The new distances
#' @noRd
update_distances <- function(features, centers, distances, cluster_i, cluster_j) {
  for (k in c(cluster_i, cluster_j)) {
    distances[, k] <- colSums((t(features) - centers[k,])^2)
  }
  distances
}

#' Update a cluster center after swapping two elements
#'
#' @param centers The current cluster centers
#' @param features The features
#' @param i the index of the first element to be swapped
#' @param j the index of the second element to be swapped
#' @param cluster_i the cluster of element i
#' @param cluster_j the cluster of element j
#' @param tab A table of the cluster frequencies
#'
#' @details
#'
#' This should make the fast exchange method much faster, because
#' most time is spent on finding the cluster centers. After swapping
#' only two elements, it should be possible to update the two centers
#' very fast
#' @noRd

update_centers <- function(centers, features, i, j, cluster_i, cluster_j, tab) {
  ## First cluster: item i is removed, item j is added
  centers[cluster_i, ] <- centers[cluster_i, ] - (features[i, ] / tab[cluster_i]) + (features[j, ] / tab[cluster_j])
  ## Other cluster: item j is removed, item i is added
  centers[cluster_j, ] <- centers[cluster_j, ] + (features[i, ] / tab[cluster_i]) - (features[j, ] / tab[cluster_j])
  centers
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
