
# I suggest running this script on the command line via Rscript "BILS.R"
# Repeat until all data sets were processed

library(anticlust)
source("BILS_METHODS.R")

RUNS_MBPI <- 5
BATCH_SIZE_SIMULATION <- 1

# Do not do the entire simulation in a single R session and adjust BATCH_SIZE_SIMULATION accordingly

for (K in 2:7) {
  files <- list.files(paste0("./datasets/K", K, "/"), full.names = FALSE)
  # Do not replicate previous files:
  if (file.exists("results_bils.csv")) {
    files_processed <- read.csv("results_bils.csv", sep = ";")$file
    already_processed <- gsub(".csv", replacement = "", x = files) %in% files_processed
    files <- files[!already_processed]
  }
  files <- sample(files, size = min(BATCH_SIZE_SIMULATION, length(files)))
  for (i in 1:length(files)) {
    file <- files[i]
    data <- read.csv(paste0("./datasets/K", K, "/", file))
    cat("Working on file ", file, ", K =", K, "\n")
    for (separate_dispersion_distances in c(FALSE, TRUE)) {
      N <- nrow(data)
      if (separate_dispersion_distances) { # optimize dispersion on other distances
        # randomly select other file with the same N, compute distances based on this
        other_file <- sample(list.files(paste0("./datasets/K", K, "/"), pattern = paste0("N", N, "_"), full.names = TRUE), 1)
        dispersion_distances <- dist(read.csv(other_file))
      } else {
        dispersion_distances <- dist(data)
      }
      
      start_vanilla <- Sys.time()
      GROUPS_BILS_VANILLA <- BILS_VANILLA(
        data, 
        K = K, 
        RUNS_MBPI = RUNS_MBPI, 
        dispersion_distances = dispersion_distances
      )
      end_vanilla <- Sys.time()
      
      solver <- ifelse(K < 5, "symphony", "Gecode")
      start <- Sys.time()
      opt <- optimal_dispersion(
        dispersion_distances, 
        K = K, 
        npartitions = RUNS_MBPI, 
        solver = solver, 
        min_dispersion_considered = dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA)
      )
      end <- Sys.time()
      cat("   Found optimal solution!\n")

      # Rerun BILS_VANILLA if the dispersion it returns is not optimal
      # do 10, 100, 1000, 10000
      
      if (dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA) != opt$dispersion) {
        for (RUNS in c(10, 100, 1000, 10000)) {
          start_vanilla <- Sys.time()
          GROUPS_BILS_VANILLA <- BILS_VANILLA(
            data, 
            K = K, 
            RUNS_MBPI = RUNS, 
            dispersion_distances = dispersion_distances
          )
          end_vanilla <- Sys.time()
          # test if optimum was found
          if (dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA) == opt$dispersion) {
            RUNS_TILL_OPTIMUM <- RUNS
            break
          } else {
            if (RUNS == 10000) {
              RUNS_TILL_OPTIMUM <- -1 # encode that no optimum was found even with 10000 repetitions
            }
          }
        }
      } else {
        RUNS_TILL_OPTIMUM <- RUNS_MBPI
      }
      
      if (RUNS_TILL_OPTIMUM > RUNS_MBPI) {
        # create new partitions, some elements are fixed through max dispersion constraints (seen in opt$groups_fixated)
        opt$groups <- t(replicate(RUNS_TILL_OPTIMUM, anticlust:::add_unassigned_elements(rep(N/K, K), opt$groups_fixated, N, K)))
      }
      
      GROUPS_BILS_E_1 <- BILS_E_1(
        data, 
        init_partition = opt$groups[1, ], 
        RUNS_MBPI = max(RUNS_TILL_OPTIMUM, RUNS_MBPI), # also run if vanilla found no optimal solution
        dispersion_distances = dispersion_distances
      )
      GROUPS_BILS_E_ALL <- BILS_E_ALL(
        data, 
        init_partitions = opt$groups, 
        dispersion_distances = dispersion_distances
      )
      GROUPS_BILS_E_ALL_RESTRICTED <- BILS_E_ALL_RESTRICTED(
        data, 
        init_partitions = opt$groups, 
        cannot_link = opt$edges,
        dispersion_distances = dispersion_distances
      )
      
      # store data associated with simulation run
      assign(paste0("df", separate_dispersion_distances), data.frame(
        file = gsub(".csv", replacement = "", x = file), 
        N = nrow(data),
        M = ncol(data),
        K = K,
        separate_dispersion_distances = as.numeric(separate_dispersion_distances),
        dispersion_distances = ifelse(separate_dispersion_distances, other_file, ""),
        DISP_VANILLA = dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA),
        DISP_E_1 = dispersion_objective(dispersion_distances, GROUPS_BILS_E_1),
        DISP_E_ALL = dispersion_objective(dispersion_distances, GROUPS_BILS_E_ALL),
        DISP_E_ALL_RESTRICTED = dispersion_objective(dispersion_distances, GROUPS_BILS_E_ALL_RESTRICTED),
        DIV_VANILLA = diversity_objective(data, GROUPS_BILS_VANILLA),
        DIV_E_1 = diversity_objective(data, GROUPS_BILS_E_1),
        DIV_E_ALL = diversity_objective(data, GROUPS_BILS_E_ALL),
        DIV_E_ALL_RESTRICTED = diversity_objective(data, GROUPS_BILS_E_ALL_RESTRICTED),
        RUNS_BILS_VANILLA = RUNS_TILL_OPTIMUM,
        time_optimal_s = as.numeric(difftime(end, start, units = "s")),
        time_vanilla_s = as.numeric(difftime(end_vanilla, start_vanilla, units = "s")),
        N_DUPLICATE_PARTITIONS = sum(duplicated(opt$groups))
      ))
    }
    df <- rbind(dfFALSE, dfTRUE)
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
