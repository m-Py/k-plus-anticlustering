## Functions for simulation

BILS_VANILLA <- function(data, K, RUNS_MBPI) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = K, 
    R = c(RUNS_MBPI, 0)
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = data)), ]
}

BILS_E_1 <- function(data, init_partition, RUNS_MBPI) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = init_partition, 
    R = c(RUNS_MBPI, 0)
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = data)), ]
}

BILS_E_ALL <- function(data, init_partitions) {
  PARTITIONS <- bicriterion_anticlustering(
    data, 
    K = init_partitions[1, ], # mostly irrelevant if `init_partitions` is passed
    R = c(nrow(init_partitions), 0),
    init_partitions = init_partitions
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = data)), ]
}
