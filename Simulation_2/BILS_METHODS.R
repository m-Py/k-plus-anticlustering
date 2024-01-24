## Functions for simulation

BILS_VANILLA <- function(data, K, RUNS_MBPI, dispersion_distances = dist(data)) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = K, 
    R = c(RUNS_MBPI, 0),
    dispersion_distances = dispersion_distances # not really VANILLA, but closely
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = dispersion_distances)), ]
}

BILS_E_1 <- function(data, init_partition, RUNS_MBPI, dispersion_distances = dist(data)) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = init_partition, 
    R = c(RUNS_MBPI, 0),
    dispersion_distances = dispersion_distances
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = dispersion_distances)), ]
}

BILS_E_ALL <- function(data, init_partitions, dispersion_distances = dist(data)) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = init_partitions[1, ], # mostly irrelevant if `init_partitions` is passed
    R = c(nrow(init_partitions), 0),
    init_partitions = init_partitions,
    dispersion_distances = dispersion_distances
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = dispersion_distances)), ]
}

BILS_E_ALL_RESTRICTED <- function(data, init_partitions, cannot_link, dispersion_distances = dist(data)) {
  distances <- as.matrix(dist(data))
  
  # set cannot-link distances to large negative value so they cannot be linked
  # during the local maximum search (no exchanges will be conducted that put
  # these into the same group)
  distances[rbind(cannot_link, t(apply(cannot_link, 1, rev)))] <- -(sum(distances) + 1)
  
  PARTITIONS <- bicriterion_anticlustering(
    distances, 
    K = init_partitions[1, ], # mostly irrelevant if `init_partitions` is passed
    R = c(nrow(init_partitions), 0),
    init_partitions = init_partitions,
    dispersion_distances = dispersion_distances
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = dispersion_distances)), ]
}
