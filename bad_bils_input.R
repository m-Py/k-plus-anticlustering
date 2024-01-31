
library(anticlust)

N <- 100
K <- 2

cannot_link <- c(1, rep(2:(N-1), each = 2), N)
cannot_link <- matrix(cannot_link, ncol = 2, byrow = TRUE)
cannot_link <- rbind(cannot_link, t(apply(cannot_link, 1, rev)))
mat <- matrix(1, nrow = N, ncol = N)
mat[cannot_link] <- -1

dispersion_objective(mat, anticlustering(mat, K = K, objective = "dispersion", method = "brusco", repetitions = 10000))

start <- Sys.time()
optimal_dispersion(mat, K = K)
Sys.time() - start
