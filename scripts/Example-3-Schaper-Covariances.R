
# Example 3

library(anticlust)

# Features for k-plus objective
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

# Features for k-covariances objective
covariance_features <- function(data) {
  M <- ncol(data)
  if (M < 2) {
    stop("k-covariances can only be used when at least two features are present.")
  }
  # number of features for equalizing covariances
  feature_combinations <- t(combn(M, 2))
  # store new features in matrix
  cov_feature_matrix <- matrix(ncol = nrow(feature_combinations), nrow = nrow(data))
  for (i in 1:nrow(feature_combinations)) {
    f1 <- feature_combinations[i, 1]
    f2 <- feature_combinations[i, 2]
    cov_feature_matrix[, i] <- (data[, f1] - mean(data[, f1])) * (data[, f2] - mean(data[, f2]))
  }
  cov_feature_matrix
}


upper_tri <- function(x) {
  dims <- dim(x)
  mat <- rep(NA, prod(dims))
  dim(mat) <- dims
  mat[upper.tri(mat)] <- x[upper.tri(x)]
  mat
}

lower_tri <- function(x) {
  dims <- dim(x)
  mat <- rep(NA, prod(dims))
  dim(mat) <- dims
  mat[lower.tri(mat)] <- x[lower.tri(x)]
  mat
}

plus_na <- function(x, y) {
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  x + y
}


features <- schaper2019[, 3:6]

extended <- cbind(features, squared_from_mean(features))
extended_cov <- cbind(extended, covariance_features(features))

ac_kplus <- anticlustering(
  scale(extended),
  K = 2,
  objective = "variance",
  method = "local-maximum",
  repetitions = 10
)

ac_cov <- anticlustering(
  scale(extended_cov),
  K = 2,
  objective = "variance",
  method = "local-maximum",
  repetitions = 10
)

cors_kplus <- lapply(by(features, ac_kplus, cor), function(x) round(x, 2))
cors_kcov <- lapply(by(features, ac_cov, cor), function(x) round(x, 2))

plus_na(upper_tri(cors_kplus[[1]]), lower_tri(cors_kplus[[2]]))
plus_na(upper_tri(cors_kcov[[1]]), lower_tri(cors_kcov[[2]]))


mean_sd_tab(features, ac_kplus)
mean_sd_tab(features, ac_cov)
