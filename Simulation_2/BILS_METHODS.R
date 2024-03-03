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

BILS_E_ALL_RESTRICTED <- function(data, init_partitions, cannot_link) {
  distances <- as.matrix(dist(data))
  
  # set cannot-link distances to large negative value so they cannot be linked
  # during the local maximum search (no exchanges will be conducted that put
  # these into the same group)
  distances[rbind(cannot_link, t(apply(cannot_link, 1, rev)))] <- -(sum(distances) + 1)
  
  PARTITIONS <- bicriterion_anticlustering(
    distances, 
    K = init_partitions[1, ], # mostly irrelevant if `init_partitions` is passed
    R = c(nrow(init_partitions), 0),
    init_partitions = init_partitions
  )
  PARTITIONS[which.max(apply(PARTITIONS, 1, dispersion_objective, x = data)), ]
}


# this one uses one half for MBPI, other half for ILS
BILS_E_ALL_RESTRICTED_ALT <- function(data, init_partitions, cannot_link) {
  distances <- as.matrix(dist(data))
  
  copy_distances <- distances
  distances[rbind(cannot_link, t(apply(cannot_link, 1, rev)))] <- -(sum(distances) + 1)
  
  half_runs <- nrow(init_partitions)/2
  
  PARTITIONS <- bicriterion_anticlustering(
    distances, 
    K = init_partitions[1, ], # mostly irrelevant if `init_partitions` is passed
    R = c(half_runs, 0),
    init_partitions = init_partitions[1:half_runs, ]
  )
  
  # rerun with ILS, using pareto set of first phase as input
  PARTITIONS2 <- bicriterion_anticlustering(
    copy_distances, 
    K = PARTITIONS[1,], # 
    R = c(nrow(PARTITIONS), half_runs),
    init_partitions = PARTITIONS
  )
  
  PARTITIONS2[which.max(apply(PARTITIONS2, 1, dispersion_objective, x = data)), ]
}


# this one uses one half for MBPI, other half for ILS
LCW_RESTRICTED_ALT <- function(data, K, RUNS, cannot_link) {
  
  half_runs <- RUNS/2
  
  GROUPS_LCW <- anticlustering(
    data, 
    K = K, 
    method = "local-maximum",
    repetitions = half_runs,
    cannot_link = cannot_link
  )

  # rerun with ILS, using pareto set of first phase as input
  PARTITIONS2 <- bicriterion_anticlustering(
    data, 
    K = GROUPS_LCW, # 
    R = c(1, half_runs)
  )
  
  PARTITIONS2[which.max(apply(PARTITIONS2, 1, dispersion_objective, x = data)), ]
}
