
# Author: Martin Papenberg
# Year: 2023

# First step in the simulation: Generate data
source("0-functions-generate-data.R")

sample_sizes <- 20:120 # 24 - 300
nsets <- 1000

for (K in 2:7) {
  Ns <- sample_sizes[sample_sizes %% K == 0]
  N  <- sample(Ns, replace = TRUE, size = nsets)
  M  <- sample(2:5, replace = TRUE, size = nsets)
  SD <- sample(c(1, 2, 3), replace = TRUE, size = nsets)
  # Store data sets as files
  for (i in 1:nsets) {
    generate_data(N[i], M[i], SD[i], dir = paste0("./datasets/K", K, "/"))
  }
}
