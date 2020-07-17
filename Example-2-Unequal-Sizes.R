# Unequal sizes

library(anticlust)

features <- schaper2019[, 3:6]

K <- 3
init <- sample(rep(1:3, nrow(schaper2019) * c(1/4, 1/4, 1/2)))

# Compute features for k-means-extended criterion
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

anticlusters <- anticlustering(
  scale(cbind(features, squared_from_mean(features))),
  K = init,
  objective = "variance",
  method = "local-maximum", 
  repetitions = 30
)

mean_sd_tab(features, anticlusters)
