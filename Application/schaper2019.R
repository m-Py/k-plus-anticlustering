
# Proof of concept for combined Max Dispersion + K-Plus approach
# (before the cannot-link restrictions are implemented in anticlust [on C level])

library(anticlust)

# Matrix of normalized Levenshtein distances
Levenshtein_Dist <- function(x) {
  x <- as.character(x)
  item_combinations <- t(combn(length(x), 2))
  dists <- matrix(NA, nrow = length(x), ncol = length(x))
  for (combs in 1:nrow(item_combinations)) {
    j <- item_combinations[combs, ][1]
    i <- item_combinations[combs, ][2]
    normalizer <- sum(c(nchar(x[i]), nchar(x[j])))
    dists[i, j] <- adist(x[i], x[j]) / normalizer
  }
  as.dist(dists)
}

positions_min_dist <- function(x) {
  positions_by_value(x, min)
}
positions_max_dist <- function(x) {
  positions_by_value(x, max)
}

positions_by_value <- function(x, fun = min, value = NULL) {
  tt <- as.matrix(x)
  tt[upper.tri(tt)] <- NA
  diag(tt) <- NA
  if (!is.null(value)) {
    return(which(tt == value, arr.ind = TRUE))
  }
  which(tt == fun(tt, na.rm = TRUE), arr.ind = TRUE)
}

lt <- Levenshtein_Dist(schaper2019$item)
hist(lt)
min(lt)

# observe pairs of similar orthography
schaper2019[t(positions_min_dist(lt)),] 
schaper2019[t(positions_by_value(lt, value = sort(lt)[2])), ]
schaper2019[t(positions_by_value(lt, value = sort(lt)[3])), ]
schaper2019[t(positions_by_value(lt, value = sort(lt)[4])), ]
schaper2019[t(positions_by_value(lt, value = sort(lt)[5])), ]
# Definition of Levenshtein normalization as done above seems to be reasonable

nparts <- 1000
max_disp <- optimal_dispersion(lt, K = 3)
GLOBAL_OPTIMUM_DISPERSION <- max_disp$dispersion
GLOBAL_OPTIMUM_DISPERSION

# what's the worst that happend with regard to pairwise similarity (for example)
schaper2019[t(positions_by_value(lt, value = GLOBAL_OPTIMUM_DISPERSION)[1 ,]), ]
schaper2019[t(positions_by_value(lt, value = GLOBAL_OPTIMUM_DISPERSION)), ]
### -> we can live with that!!!

# hist(lt)
# abline(v = GLOBAL_OPTIMUM_DISPERSION, col = "red", lty = 2, lwd = 2)

features <- schaper2019[, 3:6]
# Use k-plus and categorical variable (then the categorical variable is not included as
# hard constraint, may facilitate finding solution if we already have the cannot-link restrictions)
kplus_features <- scale(cbind(features, anticlust:::squared_from_mean(features), as.numeric(schaper2019$room == "bathroom")))

disp_obj <- function(clusters, x) {
  dispersion_objective(x, clusters)
}
var_obj <- function(clusters, x) {
  variance_objective(x, clusters)
}

nparts <- 10000
group_sizes <- c(32, 32, 32)
max_disp <- optimal_dispersion(lt, K = group_sizes, npartitions = nparts)
nrow(max_disp$groups)
# all(apply(max_disp$groups, 1, table) == apply(max_disp$groups, 1, table)[,1])

start <- Sys.time()
BILS_E_ALL <- bicriterion_anticlustering(
  as.matrix(dist(kplus_features)^2), 
  K = max_disp$groups[1,], 
  R = c(nparts, 1), 
  dispersion_distances = lt, 
  init_partitions = max_disp$groups,
  average_diversity = TRUE
)
Sys.time() - start 
BILS_E_ALL <- BILS_E_ALL[which.max(apply(BILS_E_ALL, 1, disp_obj, x = lt)), ]

start <- Sys.time()
BILS_E_1 <- bicriterion_anticlustering(
  as.matrix(dist(kplus_features)^2), 
  K = max_disp$groups[1,], 
  R = c(nparts, 1), 
  dispersion_distances = lt,
  average_diversity = TRUE
)
Sys.time() - start 
BILS_E_1 <- BILS_E_1[which.max(apply(BILS_E_1, 1, disp_obj, x = lt)), ]

start <- Sys.time()
BILS_VANILLA <- bicriterion_anticlustering(
  as.matrix(dist(kplus_features)^2), 
  K = group_sizes, 
  R = c(nparts, 1),
  average_diversity = TRUE, # not really "VANILLA"
  dispersion_distances = lt # not really "VANILLA"
)
Sys.time() - start 
BILS_VANILLA <- BILS_VANILLA[which.max(apply(BILS_VANILLA, 1, disp_obj, x = lt)), ]

start <- Sys.time()
LCW_RESTRICTED <- cannot_link_anticlustering(
  kplus_features, 
  init_clusters = max_disp$groups, 
  cannot_link = max_disp$edges, 
  objective = "variance",
  method = "local-maximum"
)
Sys.time() - start # about 4x as fast as BILS with new implementation (for 10000 repetitions)

dispersion_objective(lt, LCW_RESTRICTED)
dispersion_objective(lt, BILS_VANILLA)
dispersion_objective(lt, BILS_E_1) # sometimes not optimal for unequal-sized groups!? BUG!!
dispersion_objective(lt, BILS_E_ALL)
variance_objective(kplus_features, LCW_RESTRICTED) 
variance_objective(kplus_features, BILS_VANILLA)
variance_objective(kplus_features, BILS_E_1)
variance_objective(kplus_features, BILS_E_ALL)

## Problems for unequal-sized groups when using BILS-E

# best found solution so far:
# 854.8995 for 10000 repetitions using restricted LCW (so using forbidden edges may be useful; implement in BILS-E!)
# 854.9021 for 10000+1 repetitions using BILS-E-ALL (not using forbidden edges!)

mean_sd_tab(schaper2019[,3:6], LCW_RESTRICTED)
mean_sd_tab(schaper2019[,3:6], BILS_VANILLA)
mean_sd_tab(schaper2019[,3:6], BILS_E_1)
mean_sd_tab(schaper2019[,3:6], BILS_E_ALL)
