
# I suggest running this script on the command line via Rscript "BILS.R"
# Repeat until all data sets were processed

library(anticlust)
source("BILS_METHODS.R")

TOTAL_REPETITIONS_HEURISTICS <- 100
BATCH_SIZE_SIMULATION <- 50

# Do not do the entire simulation in a single R session and adjust BATCH_SIZE_SIMULATION accordingly

for (K in 7:2) {
  files <- list.files(paste0("./datasets/K", K, "/"), full.names = FALSE)
  # Do not replicate previous files:
  if (file.exists("results_bils.csv")) {
    files_processed <- read.csv("results_bils.csv", sep = ";")$file
    already_processed <- gsub(".csv", replacement = "", x = files) %in% files_processed
    files <- files[!already_processed]
  }
  files <- sample(files, size = min(BATCH_SIZE_SIMULATION, length(files)))
  for (i in seq_along(files)) {
    file <- files[i]
    data <- read.csv(paste0("./datasets/K", K, "/", file))
    cat("Working on file ", file, ", K =", K, "\n")
    N <- nrow(data)
    cat("   Running VANILLA with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    start_vanilla <- Sys.time()
    GROUPS_BILS_VANILLA <- BILS_VANILLA(
      data, 
      K = K, 
      RUNS_MBPI = TOTAL_REPETITIONS_HEURISTICS/2
    )
    end_vanilla <- Sys.time()
    
    solver <- ifelse(K < 5, "symphony", "Gecode")
    start <- Sys.time()
    opt <- optimal_dispersion(
      data, 
      K = K, 
      npartitions = TOTAL_REPETITIONS_HEURISTICS, 
      solver = solver, 
      min_dispersion_considered = dispersion_objective(data, GROUPS_BILS_VANILLA)
    )
    end <- Sys.time()
    cat("   Exact method found optimal solution!\n")
    
    cat("   Running E_1 with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_1 <- BILS_E_1(
      data, 
      init_partition = opt$groups[1, ], 
      RUNS_MBPI = TOTAL_REPETITIONS_HEURISTICS
    )
    cat("   Running E_1_ILS with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_1_ILS <- BILS_E_1_ILS(
      data, 
      init_partition = opt$groups[1, ], 
      RUNS_MBPI = TOTAL_REPETITIONS_HEURISTICS/2
    )
    cat("   Running E_ALL with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_ALL <- BILS_E_ALL(
      data, 
      init_partitions = opt$groups
    )
    cat("   Running E_ALL_ILS with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_ALL_ILS <- BILS_E_ALL_ILS(
      data, 
      init_partitions = opt$groups
    )
    cat("   Running E_ALL_RESTRICTED with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_ALL_RESTRICTED <- BILS_E_ALL_RESTRICTED(
      data, 
      init_partitions = opt$groups, 
      cannot_link = opt$edges
    )
    cat("   Running E_ALL_RESTRICTED_ILS with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_BILS_E_ALL_RESTRICTED_ILS <- BILS_E_ALL_RESTRICTED_ILS(
      data, 
      init_partitions = opt$groups, 
      cannot_link = opt$edges
    )
    cat("   Running LCW_RESTRICTED with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    start_lcw <- Sys.time()
    GROUPS_LCW <- anticlustering(
      data, 
      K = K, 
      method = "local-maximum",
      repetitions = TOTAL_REPETITIONS_HEURISTICS,
      cannot_link = opt$edges
    )
    end_lcw <- Sys.time()
    cat("   Running LCW_RESTRICTED_ILS with", TOTAL_REPETITIONS_HEURISTICS, "repetitions\n")
    GROUPS_LCW_ILS <- LCW_RESTRICTED_ILS(
      data, 
      K = K, 
      RUNS = TOTAL_REPETITIONS_HEURISTICS,
      cannot_link = opt$edges
    )
    
    # store data associated with simulation run
    df <- data.frame(
      file = gsub(".csv", replacement = "", x = file), 
      N = nrow(data),
      M = ncol(data),
      K = K,
      DISP_VANILLA = dispersion_objective(data, GROUPS_BILS_VANILLA),
      DISP_E_1 = dispersion_objective(data, GROUPS_BILS_E_1),
      DISP_E_1_ILS = dispersion_objective(data, GROUPS_BILS_E_1_ILS),
      DISP_E_ALL = dispersion_objective(data, GROUPS_BILS_E_ALL),
      DISP_E_ALL_ILS = dispersion_objective(data, GROUPS_BILS_E_ALL_ILS),
      DISP_E_ALL_RESTRICTED = dispersion_objective(data, GROUPS_BILS_E_ALL_RESTRICTED),
      DISP_E_ALL_RESTRICTED_ILS = dispersion_objective(data, GROUPS_BILS_E_ALL_RESTRICTED_ILS),
      DISP_LCW = dispersion_objective(data, GROUPS_LCW),
      DISP_LCW_ILS = dispersion_objective(data, GROUPS_LCW_ILS),
      DIV_VANILLA = diversity_objective(data, GROUPS_BILS_VANILLA),
      DIV_E_1 = diversity_objective(data, GROUPS_BILS_E_1),
      DIV_E_1_ILS = diversity_objective(data, GROUPS_BILS_E_1_ILS),
      DIV_E_ALL = diversity_objective(data, GROUPS_BILS_E_ALL),
      DIV_E_ALL_ILS = diversity_objective(data, GROUPS_BILS_E_ALL_ILS),
      DIV_E_ALL_RESTRICTED = diversity_objective(data, GROUPS_BILS_E_ALL_RESTRICTED),
      DIV_E_ALL_RESTRICTED_ILS = diversity_objective(data, GROUPS_BILS_E_ALL_RESTRICTED_ILS),
      DIV_LCW = diversity_objective(data, GROUPS_LCW),
      DIV_LCW_ILS = diversity_objective(data, GROUPS_LCW_ILS),
      time_optimal_s = as.numeric(difftime(end, start, units = "s")),
      time_vanilla_s = as.numeric(difftime(end_vanilla, start_vanilla, units = "s")),
      time_lcw_s = as.numeric(difftime(end_lcw, start_lcw, units = "s")),
      N_DUPLICATE_PARTITIONS = sum(duplicated(opt$groups))
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
