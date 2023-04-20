
# Author: Martin Papenberg
# Year: 2023

#' Compute objective values for a given method and data set
#' 
#' This function works with the files results-K2-solutions.csv / 
#' results-K3-solutions.csv and assumes that the input is a row
#' in one of these files.
#' 
#' A `row` is a character vector.
#' 

compute_objectives <- function(row, K) {
  filename <- paste0("./datasets/", row["file"], ".csv")
  #print(filename)
  data <- read.csv(filename)
  anticlusters <- anticlusters_from_string(row["result"])
  means_obj <- var_means(anticlusters, data)
  sd_obj <- var_sds(anticlusters, data)
  skew_obj <- var_skew(anticlusters, data)
  kur_obj <- var_kurtosis(anticlusters, data)
  cor_obj <- var_cors(anticlusters, data)
  
  return(c(
    row["ID"],
    row["method"],
    means_obj = means_obj,
    sd_obj = sd_obj,
    skew_obj = skew_obj,
    kur_obj = kur_obj,
    cor_obj = cor_obj
  ))
}

# Get anticlusters from data file string
anticlusters_from_string <- function(string) {
  c(sapply(strsplit(as.character(string), ","), as.numeric))
}

#' Quantify set similarity by discrepancy in feature means 
#' @param clusters A vector of cluster assignments
#' @param data A N x M table of item features
#' 
#' @return The objective. Lower values are better

var_means <- function(clusters, data) {
  K <- length(unique(clusters))
  featurewise_diff(by(data, clusters, colMeans), K)
}

#' Quantify set similarity by discrepancy in feature standard deviations 
#' @param clusters A vector of cluster assignments
#' @param data A N x M table of item features
#' 
#' @return The objective. Lower values are better
var_sds <- function(clusters, data) {
  K <- length(unique(clusters))
  featurewise_diff(by(data, clusters, function(x) apply(x, 2, sd)), K)
}

#' Quantify set similarity by discrepancy in Skew
#' @return The objective. Lower values are better
var_skew <- function(clusters, data) {
  K <- length(unique(clusters))
  featurewise_diff(by(data, clusters, function(x) apply(x, 2, DescTools::Skew)), K)
}

#' Quantify set similarity by discrepancy in Kurtosis
#' @return The objective. Lower values are better
var_kurtosis <- function(clusters, data) {
  K <- length(unique(clusters))
  featurewise_diff(by(data, clusters, function(x) apply(x, 2, DescTools::Kurt)), K)
}

#' Quantify set similarity by discrepancy in correlations
#' @return The objective. Lower values are better
var_cors <- function(clusters, data) {
  K <- length(unique(clusters))
  cors_per_cluster <- lapply(by(data, clusters, cor), function(x) x[lower.tri(x)])
  if (ncol(data) > 2) {
    per_cor_range_diff <- apply(simplify2array(cors_per_cluster), 1, range_diff)
  } else { # special treatment for only 2 features because the output is not a matrix when using apply
    per_cor_range_diff <- range_diff(unlist(cors_per_cluster))
  }
  mean(per_cor_range_diff)
}

# Compute per feature per cluster the range in values (mean, sd...), return
# the average across all features
featurewise_diff <- function(x, K) {
  mat <- matrix(unlist(x), ncol = K)
  mat <- apply(mat, 1, range_diff)
  mean(mat)
}

# Determine the maximum range in a vector
range_diff <- function(x) {
  diff(range(x))
}

# Compute features for k-means-extended criterion
squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}
