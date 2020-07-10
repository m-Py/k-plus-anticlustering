
# Author: Martin Papenberg
# Year: 2019-2020

# This script reproduces Application I (Stimulus assignment) in 
# "Using anticlustering to partition data sets into equivalent parts" 
# (Papenberg & Klau, 2020).

# The code can simply be executed from top to bottom. 

## 1. Load - and, if required, install - package `anticlust`

if (!requireNamespace("anticlust")) {
  if (!requireNamespace("remotes")) {
    install.packages("remotes")
  }
  remotes::install_github("m-Py/anticlust")
}

library(anticlust)

squared_from_mean <- function(data) {
  apply(data, 2, function(x) (x - mean(x))^2)
}

## 2. Set random seed for reproducibility
set.seed(1234) 

## 3. Load data (is included in the package)
data(schaper2019)

# Look at data:
head(schaper2019)
# For more information on the data set, umcomment and execute the
# following line:
# ?schaper2019

## 4. Anticlustering

# Select the four features that should be similar across 3 lists:
features <- schaper2019[, 3:6]

# Anticluster editing
ac_ace <- anticlustering(
  features, 
  K = 3, # 3 stimulus sets
  categories = schaper2019$room, # balances category across lists
  objective = "diversity", 
  method = "local-maximum",
  repetitions = 10
)

# K-Means Anticlustering
ac_kmeans <- anticlustering(
  features, 
  K = 3, 
  categories = schaper2019$room,
  objective = "variance", 
  method = "local-maximum",
  repetitions = 10
)

# K-Means-Extended
ac_kmeans_extended <- anticlustering(
  scale(cbind(features, squared_from_mean(features))), 
  K = 3, 
  categories = schaper2019$room,
  objective = "variance", 
  method = "local-maximum",
  repetitions = 10
)

## 5. Evaluate results

# Compare feature means and standard deviations
# Anticluster editing
mean_sd_tab(features, ac_ace)

# k-means anticlustering
mean_sd_tab(features, ac_kmeans)

mean_sd_tab(features, ac_kmeans_extended)
