
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
opt <- optimal_dispersion(lt, K = 3)
GLOBAL_OPTIMUM_DISPERSION <- opt$dispersion
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
kplus_features <- cbind(kplus_moment_variables(features, 2), categories_to_binary(schaper2019$room))
kplus_distances <- as.matrix(dist(kplus_features)^2)

nparts <- 5000
group_sizes <- c(32, 32, 32)
opt <- optimal_dispersion(lt, K = group_sizes, npartitions = nparts)
nrow(opt$groups)
# all(apply(max_disp$groups, 1, table) == apply(max_disp$groups, 1, table)[,1])
sum(duplicated(opt$groups))


start <- Sys.time()
BILS_E_ALL <- bicriterion_anticlustering(
  kplus_distances, 
  K = opt$groups[1,], 
  R = c(nparts, 1), 
  dispersion_distances = lt, 
  init_partitions = opt$groups,
  average_diversity = TRUE
)
Sys.time() - start 

ALL_CRITERIA <- list()
ALL_CRITERIA$DISPERSIONS <- list()
ALL_CRITERIA$DIVERSITIES <- list()

ALL_CRITERIA$DISPERSIONS$BILS_E_ALL <- apply(BILS_E_ALL, 1, dispersion_objective, x = lt)
ALL_CRITERIA$DIVERSITIES$BILS_E_ALL <- apply(BILS_E_ALL, 1, diversity_objective, x = kplus_distances)

start <- Sys.time()
BILS_E_1 <- bicriterion_anticlustering(
  kplus_distances, 
  K = opt$groups[1,], 
  R = c(nparts, 1), 
  dispersion_distances = lt,
  average_diversity = TRUE
)
Sys.time() - start 

ALL_CRITERIA$DISPERSIONS$BILS_E_1 <- apply(BILS_E_1, 1, dispersion_objective, x = lt)
ALL_CRITERIA$DIVERSITIES$BILS_E_1 <- apply(BILS_E_1, 1, diversity_objective, x = kplus_distances)


start <- Sys.time()
BILS_VANILLA <- bicriterion_anticlustering(
  kplus_distances, 
  K = group_sizes, 
  R = c(nparts, 1),
  average_diversity = TRUE, # not really "VANILLA"
  dispersion_distances = lt # not really "VANILLA"
)
Sys.time() - start 

ALL_CRITERIA$DISPERSIONS$BILS_VANILLA <- apply(BILS_VANILLA, 1, dispersion_objective, x = lt)
ALL_CRITERIA$DIVERSITIES$BILS_VANILLA <- apply(BILS_VANILLA, 1, diversity_objective, x = kplus_distances)


tmp_dists <- as.matrix(lt)
tmp_dists[rbind(opt$edges, t(apply(opt$edges, 1, rev)))] <- -99999999999999 # lazy large number, can be computed exactly

E_ALL_RESTRICTED <- bicriterion_anticlustering(
  kplus_distances,
  K = opt$groups[1,],
  R = c(nparts, 1),
  init_partitions = opt$groups,
  dispersion_distances = tmp_dists
)
ALL_CRITERIA$DISPERSIONS$E_ALL_RESTRICTED <- apply(E_ALL_RESTRICTED, 1, dispersion_objective, x = lt)
ALL_CRITERIA$DIVERSITIES$E_ALL_RESTRICTED <- apply(E_ALL_RESTRICTED, 1, diversity_objective, x = kplus_distances)

# VANILLA FOUND OPTIMUM?
max(ALL_CRITERIA$DISPERSIONS$BILS_VANILLA) == opt$dispersion

## PLOT PARETO SETS

plot(
  ALL_CRITERIA$DISPERSIONS$E_ALL_RESTRICTED,
  ALL_CRITERIA$DIVERSITIES$E_ALL_RESTRICTED,
  col = "#ABCDEF",
  cex = 1.2,
  pch = 17,
  ylim = c(min(unlist(ALL_CRITERIA$DIVERSITIES)), max(unlist(ALL_CRITERIA$DIVERSITIES))),
  xlim = c(min(unlist(ALL_CRITERIA$DISPERSIONS)), max(unlist(ALL_CRITERIA$DISPERSIONS))),
  main = "5000 iterations for MBPI",
  ylab = "Diversity",
  xlab = "Dispersion"
)

points(
  sort(ALL_CRITERIA$DISPERSIONS$BILS_VANILLA),
  ALL_CRITERIA$DIVERSITIES$BILS_VANILLA[order(ALL_CRITERIA$DISPERSIONS$BILS_VANILLA)],
  col = "red",
  pch = 19,
  cex = .7,
  type = "b"
)

points(
  sort(ALL_CRITERIA$DISPERSIONS$BILS_E_ALL),
  ALL_CRITERIA$DIVERSITIES$BILS_E_ALL[order(ALL_CRITERIA$DISPERSIONS$BILS_E_ALL)],
  col = "orange",
  pch = 25,
  cex = 1,
  bg = "orange",
  type = "b"
)

points(
  sort(ALL_CRITERIA$DISPERSIONS$BILS_E_1),
  ALL_CRITERIA$DIVERSITIES$BILS_E_1[order(ALL_CRITERIA$DISPERSIONS$BILS_E_1)],
  col = "green",
  pch = 17,
  type = "b",
  cex = 1,
  bg = "green"
)

legend("bottomleft", legend = c("VANILLA", "E_ALL_RESTRICTED", "E_ALL", "E_1"),
       pch = c(19, 17, 25), col = c("red", "#ABCDEF", "orange", "green"),
       pt.bg = c("red", "#ABCDEF", "orange", "green"), cex = 1.2)


