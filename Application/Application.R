# https://osf.io/w8jzn/ # OFS project page for paper
# https://osf.io/y7efm

library(anticlust)

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

lt <- Levenshtein_Dist(df$Item)
hist(lt)
min(lt)

# observe pairs of similar orthography
df[t(positions_min_dist(lt)),]
df[t(positions_by_value(lt, value = sort(lt)[2])), ]
df[t(positions_by_value(lt, value = sort(lt)[4])), ]
df[t(positions_by_value(lt, value = sort(lt)[5])), ]
df[t(positions_by_value(lt, value = sort(lt)[7])), ]
# Definition of Levenshtein normalization as done above seems to be reasonable

bathroom <- subset(df, Scene == "bathroom")
bathroom_dists <- Levenshtein_Dist(bathroom$Item)
runs <- 5000
opt <- optimal_dispersion( # E ALL NOT GOOD HERE?!
  bathroom_dists,
  K = 3
)

1:nrow(bathroom) %in% unique(opt$edges) # !!

features <- bathroom[, c("ConsistencyRating", "InconsistencyRating_Bedroom",
                         "InconsistencyRating_Kitchen", "Syllables", "Frequency")]

kplus_features <- kplus_moment_variables(features, 2)

pareto_set_E_1 <- bicriterion_anticlustering( # E_1
  kplus_features,
  K = opt$groups,
  R = c(runs, runs),
  dispersion_distances = bathroom_dists
)

diversities_pareto1 <- apply(pareto_set_E_1, 1, diversity_objective, x = kplus_features)
dispersions_pareto1 <- apply(pareto_set_E_1, 1, dispersion_objective, x = bathroom_dists)

pareto_set_VANILLA <- bicriterion_anticlustering(
  kplus_features,
  K = 3,
  R = c(runs, runs),
  dispersion_distances = bathroom_dists
)

diversities_pareto2 <- apply(pareto_set_VANILLA, 1, diversity_objective, x = kplus_features)
dispersions_pareto2 <- apply(pareto_set_VANILLA, 1, dispersion_objective, x = bathroom_dists)

tmp_dists <- as.matrix(bathroom_dists)
tmp_dists[rbind(opt$edges, t(apply(opt$edges, 1, rev)))] <- -99999999999999

pareto_set_E_RESTRICTED <- bicriterion_anticlustering(
  kplus_features,
  K = opt$groups,
  R = c(runs, runs),
  dispersion_distances = tmp_dists
)

diversities_pareto3 <- apply(pareto_set_E_RESTRICTED, 1, diversity_objective, x = kplus_features)
dispersions_pareto3 <- apply(pareto_set_E_RESTRICTED, 1, dispersion_objective, x = bathroom_dists)

# Plot the pareto set
plot(
  dispersions_pareto1,
  diversities_pareto1,
  col = "#ABCDEF",
  cex = 3,
  pch = 17,
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
  dispersions_pareto2,
  diversities_pareto2,
  col = "red",
  pch = 19
)

points(
  dispersions_pareto3,
  diversities_pareto3,
  col = "orange",
  pch = 25,
  cex = 3,
  bg = "orange"
)

legend("bottomleft", legend = c("VANILLA", "E_1", "E_1_RESTRICTED"),
       pch = c(19, 17, 25), col = c("red", "#ABCDEF", "orange"),
       pt.bg = c("red", "#ABCDEF", "orange"), cex = 3)

# Do all methods have optimal dispersion?
max(dispersions_pareto1) == opt$dispersion
max(dispersions_pareto2) == opt$dispersion
max(dispersions_pareto3) == opt$dispersion

# Select partition!
all_sets <- list(diversities_pareto1, diversities_pareto2, diversities_pareto3)

paretosets <- data.frame(
  Dispersion = c(dispersions_pareto1, dispersions_pareto2, dispersions_pareto3),
  Diversity = c(diversities_pareto1, diversities_pareto2, diversities_pareto3),
  Method = rep(c("E_1", "VANILLA", "E1_RESTRICTED"), lengths(all_sets)),
  Index = unlist(lapply(sapply(lengths(all_sets), ":", 1), rev)) # urg
)

paretosets |> arrange(-Diversity)

# RESTRICTED METHOD IS NOT GOOD IF THE NUMBER OF UNIQUE PARTITIONS IS SMALL

best_e1 <- pareto_set_E_1[7, ]  # HARD CODED!!!!!!

bathroom[t(positions_by_value(bathroom_dists, value = .32)), ] # seems a good value

best_diversity <- pareto_set_E_1[3, ] # HARD CODED!!!!!!

dispersion_objective(bathroom_dists, best_diversity) # minimum
bathroom[t(positions_by_value(bathroom_dists, value = dispersion_objective(bathroom_dists, best_diversity))), ]

mean_sd_tab(features, best_diversity)
mean_sd_tab(features, best_e1)


# Arguably: Optimal dispersion not needed here! But: Using k-plus and pairwise orthographic
# dissimilarity is neat! Using partition with improved dispersion does not really come at
# the cost of decreased between-set similarity!

