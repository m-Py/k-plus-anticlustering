# https://osf.io/w8jzn/ # OFS project page for paper
# https://osf.io/y7efm

library(anticlust) # v _0.8.2.9999 currently
library(tidyr)
library(dplyr)

file <- ifelse(file.exists("Materials Experiment 3.csv"), "Materials Experiment 3.csv", "https://osf.io/download/y7efm/")

df <- read.csv(file)

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

subdf <- subset(df, Scene == "bedroom")
bedroom_dists <- as.matrix(Levenshtein_Dist(subdf$Item))
runs <- 500
opt <- optimal_dispersion( # E ALL NOT GOOD HERE?!
  bedroom_dists,
  K = 3,
  npartitions = runs
)

sum(duplicated(opt$groups))

1:nrow(subdf) %in% unique(opt$edges) # !!

features <- subdf[, c("ConsistencyRating", "InconsistencyRating_Kitchen",
                         "InconsistencyRating_Bathroom", "Syllables", "Frequency")]

kplus_features <- kplus_moment_variables(features, 2)

pareto_set_E_ALL <- bicriterion_anticlustering( # E_ALL
  kplus_features,
  K = opt$groups[1, ],
  R = c(runs, runs),
  init_partitions = opt$groups,
  dispersion_distances = bedroom_dists
)

diversities_pareto1 <- apply(pareto_set_E_ALL, 1, diversity_objective, x = kplus_features)
dispersions_pareto1 <- apply(pareto_set_E_ALL, 1, dispersion_objective, x = bedroom_dists)

pareto_set_VANILLA <- bicriterion_anticlustering(
  kplus_features,
  K = 3,
  R = c(runs, runs),
  dispersion_distances = bedroom_dists
)

diversities_pareto2 <- apply(pareto_set_VANILLA, 1, diversity_objective, x = kplus_features)
dispersions_pareto2 <- apply(pareto_set_VANILLA, 1, dispersion_objective, x = bedroom_dists)

tmp_dists <- as.matrix(bedroom_dists)
tmp_dists[rbind(opt$edges, t(apply(opt$edges, 1, rev)))] <- -99999999999999

pareto_set_E_RESTRICTED <- bicriterion_anticlustering(
  kplus_features,
  K = opt$groups[1, ],
  R = c(runs, runs),
  init_partitions = opt$groups,
  dispersion_distances = tmp_dists
)

diversities_pareto3 <- apply(pareto_set_E_RESTRICTED, 1, diversity_objective, x = kplus_features)
dispersions_pareto3 <- apply(pareto_set_E_RESTRICTED, 1, dispersion_objective, x = bedroom_dists)

# Do all methods have optimal dispersion?
max(dispersions_pareto1) == opt$dispersion
max(dispersions_pareto2) == opt$dispersion
max(dispersions_pareto3) == opt$dispersion

# Plot the pareto set
plot(
  sort(dispersions_pareto1),
  diversities_pareto1[order(dispersions_pareto1)],
  col = "#ABCDEF",
  cex = 1.2,
  pch = 17,
  lty = 1,
  type = "b",
  xlim = c(
    min(dispersions_pareto1, dispersions_pareto2, dispersions_pareto3),
    max(dispersions_pareto1, dispersions_pareto2, dispersions_pareto3)
  ),
  ylim = c(
    min(diversities_pareto1, diversities_pareto2, diversities_pareto3),
    max(diversities_pareto1, diversities_pareto2, diversities_pareto3)
  )
)

points(
  sort(dispersions_pareto2),
  diversities_pareto2[order(dispersions_pareto2)],
  col = "red",
  type = "b",
  pch = 19,
  lty = 2,
  cex = .7
)

points(
  dispersions_pareto3,
  diversities_pareto3,
  col = "orange",
  pch = 25,
  cex = 1.5,
  bg = "orange"
)

legend("bottomleft", legend = c("VANILLA", "E_1", "E_1_RESTRICTED"),
       pch = c(19, 17, 25), col = c("red", "#ABCDEF", "orange"),
       pt.bg = c("red", "#ABCDEF", "orange"), cex = 1.2)

all_partitions <- list(
  E_1 = pareto_set_E_ALL,
  VANILLA = pareto_set_VANILLA,
  E1_RESTRICTED = pareto_set_E_RESTRICTED
)

# Select partition!
all_sets <- list(E_1 = diversities_pareto1, VANILLA = diversities_pareto2, E1_RESTRICTED = diversities_pareto3)

paretosets <- data.frame(
  Dispersion = c(dispersions_pareto1, dispersions_pareto2, dispersions_pareto3),
  Diversity = c(diversities_pareto1, diversities_pareto2, diversities_pareto3),
  Method = rep(c("E_1", "VANILLA", "E1_RESTRICTED"), lengths(all_sets)),
  Index = unlist(lapply(sapply(lengths(all_sets), ":", 1), rev)), # urg
  row.names = NULL
)

paretosets |> arrange(-Diversity)

diversity_objective(kplus_features, opt$groups[1, ])

target_grouping <- paretosets |> 
  filter(Diversity >= 0) |> 
  filter(Dispersion == max(Dispersion)) 

target_method <- target_grouping[1, "Method"]
target_index <- target_grouping[1, "Index"]
target_dispersion <- target_grouping[1, "Dispersion"]

selected_partition <- all_partitions[[target_method]][target_index, ]

worst_cases <- subdf[t(positions_by_value(bedroom_dists, value = target_dispersion)), ] # seems a good value
worst_cases$Match <- 1:2
worst_cases[, c("Item", "Match")] # not at all similar to each other

dispersion_objective(bedroom_dists, selected_partition)
diversity_objective(kplus_features, selected_partition)

mean_sd_tab(features, selected_partition)

selected_partition_bicriterion <- selected_partition

# compare to best partition according to diversity alone!

best_grouping <- paretosets |>
  filter(Diversity == max(Diversity)) 

target_method <- best_grouping[1, "Method"]
target_index <- best_grouping[1, "Index"]
target_dispersion <- best_grouping[1, "Dispersion"]

selected_partition <- all_partitions[[target_method]][target_index, ]

worst_cases <- subdf[t(positions_by_value(bedroom_dists, value = target_dispersion)), ] # seems a good value
worst_cases$Match <- 1:2
worst_cases[, c("Item", "Match")] # not at all similar to each other

dispersion_objective(bedroom_dists, selected_partition) # minimum
diversity_objective(kplus_features, selected_partition)

# Rather similar, best diversity maximizing partition slightly better
# with regard to consistency / inconsistency ratings
mean_sd_tab(features, selected_partition, return_diff = TRUE)
mean_sd_tab(features, selected_partition_bicriterion, return_diff = TRUE)
mean_sd_tab(features, sample(selected_partition_bicriterion), return_diff = TRUE)

# Arguably: Optimal dispersion not needed here! But: Using k-plus and pairwise orthographic
# dissimilarity is neat! Using partition with improved dispersion does not really come at
# the cost of decreased between-set similarity!
