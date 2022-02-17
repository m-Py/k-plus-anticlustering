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

foo <- function() {

    features <- paired_data(n = 60, r = .3)

    K <- 4
    
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

    return(
        c(
            random = diff_group_cors(features, sample(anticlusters1)),
            kmeans = diff_group_cors(features, anticlusters1),
            kplus = diff_group_cors(features, anticlusters2),
            diversity = diff_group_cors(features, anticlusters3),
            kcov = diff_group_cors(features, anticlusters4)
        )
    )
}


rowMeans(replicate(1000, foo())) # lowest deviation for diversity!

