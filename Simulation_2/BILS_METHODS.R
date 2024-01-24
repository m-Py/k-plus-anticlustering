## Functions for simulation

BILS_VANILLA <- function(data, K, RUNS_MBPI, dispersion_distances = dist(data)) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = K, 
    R = c(RUNS_MBPI, 0),
    dispersion_distances = dispersion_distances
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
