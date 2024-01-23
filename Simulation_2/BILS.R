
library(anticlust)
source("BILS_METHODS.R")

RUNS_MBPI <- 5

files <- list.files("./datasets", full.names = FALSE)

for (i in 1:length(files)) {
  file <- files[i]
  cat(i, "'th iteration, working on ", file, "\n")
  data <- read.csv(paste0("./datasets/", file))
  
  for (K in 2:4) {
    start <- Sys.time()
    opt <- optimal_dispersion(data, K = K, npartitions = RUNS_MBPI)
    end <- Sys.time()

    GROUPS_BILS_VANILLA <- BILS_VANILLA(data, K = K, RUNS_MBPI = RUNS_MBPI)
    GROUPS_BILS_E_1 <- BILS_E_1(data, init_partition = opt$groups[1, ], RUNS_MBPI = RUNS_MBPI)
    GROUPS_BILS_E_ALL <- BILS_E_ALL(data, init_partitions = opt$groups)
    
    df <- data.frame(
      file = gsub(".csv", replacement = "", x = file), 
      N = nrow(data),
      M = ncol(data),
      K = K,
      DISP_VANILLA = dispersion_objective(data, GROUPS_BILS_VANILLA),
      DISP_E_1 = dispersion_objective(data, GROUPS_BILS_E_1),
      DISP_E_ALL = dispersion_objective(data, GROUPS_BILS_E_ALL),
      DIV_VANILLA = diversity_objective(data, GROUPS_BILS_VANILLA),
      DIV_E_1 = diversity_objective(data, GROUPS_BILS_E_1),
      DIV_E_ALL = diversity_objective(data, GROUPS_BILS_E_ALL),
      time_optimal_s = as.numeric(difftime(end, start, units = "s"))
    )
    results_file_exists <- FALSE
    if (file.exists("results_bils.csv")) {
      results_file_exists <- TRUE
    }
    write.table(
      df, 
      file = "results_bils.csv", 
      append = results_file_exists, 
      col.names = !results_file_exists,
      row.names = FALSE, 
      quote = FALSE,
      sep = ";"
    )
  }
}

