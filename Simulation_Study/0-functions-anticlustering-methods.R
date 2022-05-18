
# Author: Martin Papenberg
# Year: 2022

#' Apply several anticlustering methods to a data set
#' 
#' @param file The name of the file to be analyzed
#' @param path The path to the file to be analyzed
#' @param K The number of groups to be created
#' 
#' @return A data frame that contains the anticluster assignment per method
#'     Primarily, however, this function is called to write the results to
#'     a file.
#' 

anticluster_data <- function(file, path, K) {
  message("Processing file `", file, "` with K = ", K)
  data <- read.csv(paste0(path, file))
  clusters <- anticluster_data_(data, K)
  # Convert data to data.frame
  clusters <- data.frame(
    method = names(clusters), 
    result = sapply(clusters, toString)
  )
  ## Add information on simulation run
  infos <- info_from_filename(file)
  clusters <- data.frame(infos, clusters)
  rownames(clusters) <- NULL
  
  ## Write the results to file
  results_file <- paste0("results-", paste0("K", K), "-solutions.csv")
  if (file.exists(results_file)) {
    # Ensure that the same data file is not processed again 
    # (should not happen anyway, but be sure of that)
    results <- read.csv2(results_file, stringsAsFactors = FALSE)
    if (any(grepl(clusters$ID[1], results$ID))) {
      message("Warning: file ", clusters$ID[1], " was already processed")
      return(NULL)
    }
    write.table(clusters, results_file, row.names = FALSE, sep = ";", 
                append = TRUE, col.names = FALSE)
  } else {
    write.table(clusters, results_file, row.names = FALSE, sep = ";", 
                append = FALSE, col.names = TRUE)
  }
  clusters
}

# Internal function that actually calls the anticlustering methods
anticluster_data_ <- function(data, K) {
  
  N <- nrow(data)

  ## Generate data matrix for storing the objective values
  objectives <- c(
    "diversity",
    "k-means",
    "kplus",
    "kplus-skew-kurtosis",
    "kplus-correlation",
    "random"
  )
  clusters <- as.list(rep(NA, length(objectives)))
  names(clusters) <- objectives
  repetitions <- 5
  method <- "local-maximum"
  
  standardize <- TRUE
  
  ## Apply methods
  for (i in objectives) {
    if (i == "diversity") {
      clusters[[i]] <- anticlustering(
        data, 
        K = K,
        objective = "diversity",
        method = method,
        repetitions = repetitions,
        standardize = standardize
      )
    } else if (i == "k-means") {
      clusters[[i]] <- anticlustering(
        data, 
        K = K,
        objective = "variance",
        method = method,
        repetitions = repetitions,
        standardize = standardize
      )
    } else if (i == "kplus") {
      clusters[[i]] <- anticlustering(
        data, 
        K = K,
        objective = "kplus",
        method = method,
        repetitions = repetitions,
        standardize = standardize
      )
    } else if (i == "kplus-skew-kurtosis") {
      var_data <- moment_features(data, moment = 2)
      skew_data <- moment_features(data, moment = 3)
      kurtosis_data <- moment_features(data, moment = 4)
      data_skew_kurtosis <- scale(cbind(data, var_data, skew_data, kurtosis_data))
        clusters[[i]] <- anticlustering(
          data_skew_kurtosis, 
          K = K,
          objective = "variance",
          method = method,
          repetitions = repetitions,
          standardize = FALSE
        )
    } else if (i == "kplus-correlation") {
      var_data <- moment_features(data, moment = 2)
      covar_data <- covariance_features(data)
      data_correlation <- scale(cbind(data, var_data, covar_data))
      clusters[[i]] <- anticlustering(
        data_correlation, 
        K = K,
        objective = "variance",
        method = method,
        repetitions = repetitions,
        standardize = FALSE
      )
    } else if (i == "random") {
      clusters[[i]] <- sample(rep_len(1:K, N))
    }
  }
  clusters
}

# function to compute features for variance
# moment = 2 -> variance; 3 = skew; 4 = kurtosis
moment_features <- function(data, moment = 2) {
  apply(data, 2, function(x) (x - mean(x))^moment)
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

