
# Author: Martin Papenberg
# Year: 2022

# First step in the simulation: Generate data
source("0-functions-generate-data.R")

sample_sizes <- 12 * 2:25 # 24 - 300
nsets <- 100

N  <- sample(sample_sizes, replace = TRUE, size = nsets)
M  <- sample(2:5, replace = TRUE, size = nsets)
SD <- sample(c(1, 2, 3), replace = TRUE, size = nsets)
r  <- sample(c(0, .1, .2, .3, .4, .5), replace = TRUE, size = nsets)

# Store data sets as files
for (i in 1:nsets) {
  generate_data(N[i], M[i], SD[i], r[i], dir = "./datasets/")
}
