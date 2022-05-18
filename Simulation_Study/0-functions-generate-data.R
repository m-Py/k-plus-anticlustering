
# Author: Martin Papenberg
# Year: 2022

#' Generate a data set for k-plus anticlustering simulation.
#' Uses faux::rnorm_multi to generate the multivariate normal data.
#' 
#' @param N the number of elements
#' @param M the number of features
#' @param sd The standard deviation of the distribution, passed to rnorm_multi
#' @param r The correlation between features, passed to rnorm_multi
#' 
#' @return Nothing. The data set is written to a file using a unique 
#'     ID as file name. The file name also contains information on 
#'     the input parameters that generated the data. 
#'     

generate_data <- function(N, M, sd, r, dir = "datasets-K2/") {
  data <- faux::rnorm_multi(N, M, sd = sd, r = r)
  file_name <- paste0(
    dir,
    "N", N, "_M", M, "_", "SD", sd, "_", "r", r, "_",
    paste0(sample(LETTERS, 4), sample(0:9, 4), collapse = ""),
    ".csv")
  write.table(data, file_name, row.names = FALSE, sep = ",")
  return(invisible(NULL))
}

#' Extract information on simulation run from file name
#' @param file The file name
#' @return A list of information on the data set in the simulation run
#'   (file, ID, N, M, SD, r)

info_from_filename <- function(file) {
  tests <- list()
  tests$file <- gsub(pattern = ".csv", replacement = "", x = file)
  info <- unlist(lapply(
    strsplit(tests$file, "_"), 
    FUN = gsub,
    pattern = "_", 
    replacement = ""
  ))
  tests$ID <- info[5]
  tests$N <- number_from_string(info[1])
  tests$M <- number_from_string(info[2])
  tests$SD <- number_from_string(info[3])
  tests$SD <- as.numeric(gsub("r", "", info[4]))
  tests
}

number_from_string <- function(x) gsub("[^0-9]", "", x)
