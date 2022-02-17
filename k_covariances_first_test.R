library(anticlust)

# Compute features for k-means-extended criterion
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

# Difference between minimum and maximum in a vector
diff_min_max <- function(x) {
  diff(range(x))
}


# Compute difference between a correlations in two groups
diff_group_cors <- function(features, clusters) {
  diff_min_max(by(features, clusters, function(x) cor(x[, 1], x[, 2])))
}

source("./Simulation_Study/0-functions-analyze-data.R")

paired_data <- function(n, mx = 0, my = 0, sdx = 1, sdy = 1, r, empirical = FALSE) {
  cor_matrix <- matrix(c(1, r, r, 1), ncol = 2)
  sds <- c(sdx, sdy)
  vars <- sds %*% t(sds)
  cov_matrix <- vars * cor_matrix
  data <- MASS::mvrnorm(
    n, 
    mu = c(mx, my), 
    Sigma = cov_matrix, 
    empirical = empirical
  ) 
  data <- data.frame(data)
  colnames(data) <- c("x", "y")
  data
}

sim_kcov <- function(X, N, r, K) {

    K <- K

    features <- as.matrix(paired_data(n = N, r = r))

    anticlusters1 <- anticlustering(
        features,
        objective = "variance",
        K = K
    )

    anticlusters2 <- anticlustering(
        features,
        objective = "kplus",
        K = K
    )

    anticlusters3 <- anticlustering(
        features,
        objective = "diversity",
        K = K
    )
    
    # optimize covariance similarity
    cov_features <- (features[, 1] - mean(features[,1])) * (features[, 2] - mean(features[,2]))
    var_features <- squared_from_mean(features)
    anticlusters4 <- anticlustering(
        scale(cbind(features, var_features, cov_features)),
        objective = "variance",
        K = K
    )
    
    rnd_anticlusters <- sample(anticlusters1)
    
    results <- matrix(
            c(c(
                random = var_means(rnd_anticlusters, features),
                kmeans = var_means(anticlusters1, features),
                kplus = var_means(anticlusters2, features),
                diversity = var_means(anticlusters3, features),
                kcov = var_means(anticlusters4, features)
            ),
            c(
                random = var_sds(rnd_anticlusters, features),
                kmeans = var_sds(anticlusters1, features),
                kplus = var_sds(anticlusters2, features),
                diversity = var_sds(anticlusters3, features),
                kcov = var_sds(anticlusters4, features)
            ),
            c(
                random = diff_group_cors(features, rnd_anticlusters),
                kmeans = diff_group_cors(features, anticlusters1),
                kplus = diff_group_cors(features, anticlusters2),
                diversity = diff_group_cors(features, anticlusters3),
                kcov = diff_group_cors(features, anticlusters4)
            )), ncol = 3
        )
    colnames(results) <- c("means", "sds", "cors")
    rownames(results) <- c("random", "kmeans", "kplus", "diversity", "kcov")
    
    results
}


nsim <- 500
sim <- lapply(1:nsim, sim_kcov, N = 100, r = .4, K = 5)
round(Reduce("+", sim) / nsim, 3) # average objective!

