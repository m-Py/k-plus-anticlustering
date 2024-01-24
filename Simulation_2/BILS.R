
library(anticlust)
source("BILS_METHODS.R")

RUNS_MBPI <- 5
BATCH_SIZE_SIMULATION <- 20

files <- list.files("./datasets", full.names = FALSE)

# Do not replicate previous files:
if (file.exists("results_bils.csv")) {
  files_processed <- read.csv("results_bils.csv", sep = ";")$file
  already_processed <- gsub(".csv", replacement = "", x = files) %in% files_processed
  files <- files[!already_processed]
}

# Do not do the entire simulation in a single R session and adjust BATCH_SIZE_SIMULATION accordingly
files <- sample(files, size = max(BATCH_SIZE_SIMULATION, length(files)))

for (i in 1:length(files)) {
  file <- files[i]
  cat(i, "'th iteration, working on ", file, "\n")
  data <- read.csv(paste0("./datasets/", file))
  
  for (K in 2:4) {
    for (separate_dispersion_distances in c(FALSE, TRUE)) {
      if (separate_dispersion_distances) { # optimize dispersion on other distances
        N <- nrow(data)
        other_file <- sample(list.files("./datasets", pattern = paste0("N", N, "_"), full.names = TRUE), 1)
        dispersion_distances <- dist(read.csv(other_file))
      } else {
        dispersion_distances <- dist(data)
      }
      start <- Sys.time()
      opt <- optimal_dispersion(dispersion_distances, K = K, npartitions = RUNS_MBPI)
      end <- Sys.time()
      
      GROUPS_BILS_VANILLA <- BILS_VANILLA(
        data, 
        K = K, 
        RUNS_MBPI = RUNS_MBPI, 
        dispersion_distances = dispersion_distances
      )
      # Rerun BILS_VANILLA if the dispersion it returns is not optimal
      # do 10, 100, 1000, 10000
      
      if (dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA) != opt$dispersion) {
        for (RUNS in c(10, 100, 1000, 10000)) {
          GROUPS_BILS_VANILLA_REPEATED <- BILS_VANILLA(
            data, 
            K = K, 
            RUNS_MBPI = RUNS, 
            dispersion_distances = dispersion_distances
          )
          if (dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA_REPEATED) == opt$dispersion) {
            RUNS_TILL_OPTIMUM <- RUNS
            break
          } else {
            if (RUNS == 10000) {
              RUNS_TILL_OPTIMUM <- -1
            }
          }
        }
      } else {
        RUNS_TILL_OPTIMUM <- RUNS_MBPI
      }
      
      GROUPS_BILS_E_1 <- BILS_E_1(
        data, 
        init_partition = opt$groups[1, ], 
        RUNS_MBPI = RUNS_MBPI, 
        dispersion_distances = dispersion_distances
      )
      GROUPS_BILS_E_ALL <- BILS_E_ALL(
        data, 
        init_partitions = opt$groups, 
        dispersion_distances = dispersion_distances
      )
      
      # store data associated with simulation run
      assign(paste0("df", K, separate_dispersion_distances), data.frame(
        file = gsub(".csv", replacement = "", x = file), 
        N = nrow(data),
        M = ncol(data),
        K = K,
        separate_dispersion_distances = as.numeric(separate_dispersion_distances),
        dispersion_distances = ifelse(separate_dispersion_distances, other_file, ""),
        DISP_VANILLA = dispersion_objective(dispersion_distances, GROUPS_BILS_VANILLA),
        DISP_E_1 = dispersion_objective(dispersion_distances, GROUPS_BILS_E_1),
        DISP_E_ALL = dispersion_objective(dispersion_distances, GROUPS_BILS_E_ALL),
        DIV_VANILLA = diversity_objective(data, GROUPS_BILS_VANILLA),
        DIV_E_1 = diversity_objective(data, GROUPS_BILS_E_1),
        DIV_E_ALL = diversity_objective(data, GROUPS_BILS_E_ALL),
        RUNS_BILS_VANILLA = RUNS_TILL_OPTIMUM,
        time_optimal_s = as.numeric(difftime(end, start, units = "s"))
      ))
    }
  }
  df <- rbind(df2FALSE, df2TRUE, df3FALSE, df3TRUE, df4FALSE, df4TRUE)
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
