
# Test equivalence of objectives

# Check that I can actually just append the squared difference data as 
# additional columns to the original data frame:

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}


N <- 500
M <- 20
data <- matrix(rnorm(N * M), ncol = M)
var_data <- squared_from_mean(data)

K <- 10
clusters <- sample(rep_len(1:K, N))

obj1 <- variance_objective(data, clusters) + variance_objective(var_data, clusters)
obj2 <- variance_objective(cbind(data, var_data), clusters)

tolerance <- 1e-10

obj1 - obj2 < tolerance

obj1
obj2
