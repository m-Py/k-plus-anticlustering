
# Compute features for k-means-extended criterion
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

# Get the last element of a list / vector
last <- function(x) {
  x[[length(x)]]
}

# Sum of difference between minimum and maximum value per feature of a 
# data frame, by a group variable (i.e., total deviation in variances,
# measured by difference between min/max variance, across several features)
sum_group_diff_min_max <- function(clusters, features, fun) {
  sum(group_diff_min_max(clusters, features, fun))
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


# For a data frame (rows = partitions, columns = objectives), determine which 
# elements are part of the Pareto efficient set. Output is a logical vector,
# illustrating for each row if it is part of the Pareto efficient set.
# `sense` can be "max" or "min", depending on whether high objective values
# are good or low objective values. 
pareto_set <- function(df, sense = "max") {
  if (sense == "min") {
    df <- -df
  }
  df <- data.frame(df)
  ncrits <- ncol(df)
  df$ID <- 1:nrow(df)
  df <- df[order(rowSums(df[, 1:ncrits]), decreasing = TRUE), ]
  
  is_dominated <- rep(FALSE, nrow(df)) 
  for (i in nrow(df):1) {
    for (j in 1:nrow(df)) {
      j_dominates_i <- all(df[i, 1:ncrits] <= df[j, 1:ncrits]) && 
                       any(df[i, 1:ncrits] < df[j, 1:ncrits])
      if (j_dominates_i) {
        is_dominated[df$ID[i]] <- TRUE
        break
      }
    }
  }
  !is_dominated
}
