library(anticlust)
library(effectsize)

source("0-functions-analyze-data.R")

reppp <- function(X, N, M, K) {
  dat <- matrix(rnorm(N*M), ncol = M)
  clusters1 <- anticlustering(dat, K = K, objective = "kplus", method = "local-maximum", repetitions = 50)
  clusters2 <- anticlustering(dat, K = K, objective = "kplus")
  c(
    many_repetitions_sds = var_sds(clusters1, dat),
    few_repetitions_sds = var_sds(clusters2, dat),
    many_repetitions_skew = var_skew(clusters1, dat),
    few_repetitions_skew = var_skew(clusters2, dat)
  )
}

nsim <- 100
tt <- t(sapply(1:nsim, reppp, N = 100, M = 10, K = 10))
colMeans(tt) |> round(2)

t.test(tt[,"many_repetitions_sds"], tt[, "few_repetitions_sds"], paired = TRUE)
cohens_d(tt[,"many_repetitions_sds"], tt[, "few_repetitions_sds"], paired = TRUE)

t.test(tt[,"many_repetitions_skew"], tt[, "few_repetitions_skew"], paired = TRUE)
cohens_d(tt[,"many_repetitions_skew"], tt[, "few_repetitions_skew"], paired = TRUE)

