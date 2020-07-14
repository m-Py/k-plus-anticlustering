
# Compute features for k-means-extended criterion
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

# Get the last element of a list / vector
last <- function(x) {
  x[[length(x)]]
}

# Difference between minimum and maximum value per feature of a data frame,
# by a group variable
group_diff_min_max <- function(clusters, features, fun) {
  apply(fun_by_group(features, clusters, fun), 1, diff_min_max)
}

# Function that computes a function for each feature of a data frame,
# by a grouping variable
fun_by_group <- function(features, clusters, fun) {
  data.frame(lapply(by(features, clusters, function(x) apply(x, 2, fun)), c))
}

# Difference between minimum and maximum in a vector
diff_min_max <- function(x) {
  diff(range(x))
}

# short-cut function to determine difference in minimum and maximum
# variance, across groups, for one feature
diff_vars <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, var))))
}

# short-cut function to determine difference in minimum and maximum
# means, across groups, for one feature
diff_mean <- function(clusters, features) {
  abs(diff(range(tapply(features, clusters, mean))))
}

# Variance objective function with changed order of the arguments 
# (can be called by sapply() etc.)
var_objective <- function(clusters, features) {
  anticlust::variance_objective(features, clusters)
}
