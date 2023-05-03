
library(fossil)
source("0-functions-analyze-data.R")

# Compute all pairwise ARIs for a set of partitions
# argument l is a list of partitions
all_pairwise_aris <- function(l, ID) {
  method_names <- gsub(paste0("-", ID), "", names(l))
  ARIs <- matrix(NA, nrow = length(l), ncol = length(l))
  colnames(ARIs) <- method_names
  rownames(ARIs) <- method_names
  for (i in seq_along(l)) {
    for (j in seq_along(l)) {
      ARIs[i, j] <- adj.rand.index(l[[i]], l[[j]])
    }
  }
  ARIs
}

Ks <- 2:4
results_by_k <- vector(mode = "list", length = 3)
names(results_by_k) <- Ks
for (K in Ks) {
  data <- read.csv2(paste0("results-K", K, "-solutions.csv"), stringsAsFactors = FALSE)
  
  # ensure that the order of methods is good
  n_methods <- length(unique(data$method))
  apply(matrix(data$method, ncol = n_methods, byrow = TRUE), 2, function(x) all(x == x[1]))
  all_solutions <- lapply(data$result, anticlusters_from_string)
  names(all_solutions) <- paste0(rep_len(data$method[1:n_methods], length(all_solutions)), "-", data$ID)
  
  # exclude correlation method
  all_solutions <- all_solutions[!grepl("correlation", names(all_solutions))]
  
  
  all_aris <- vector(mode = "list", length = length(unique(data$ID)))
  names(all_aris) <- unique(data$ID)
  for (id in unique(data$ID)) {
    current_solutions <- all_solutions[grepl(id, names(all_solutions))]
    all_aris[[id]] <- all_pairwise_aris(current_solutions, id)
  }
  
  results_by_k[[as.character(K)]] <- Reduce("+", all_aris) / length(all_aris)
}

write.table(Reduce("+", results_by_k) / length(results_by_k), "global_results_ari.csv", sep = ",")
