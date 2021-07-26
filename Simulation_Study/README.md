
# README

- Author: Martin Papenberg
- Year: 2021

---

This directory contains code and data to reproduce and analyze the simulation study reported in: 

XXX TODO

## General information

- The directory "./datasets" contains the 10,000 data sets used
- R scripts that start with "0-functions" are function definitions that are used throughout the simulation. These files are "sourced" from the R-scripts that start with numbers >= 1 ("1-Generate-Data.R"; "2-Call-Methods.R"; "3-Compute-Objectives.R"; "4-Aggregate-Results.R")
- The script "1-Generate-Data.R" should not be called directly because doing so would generate another 10,000 data files in the directory "./datasets", but the data sets that were used in the paper already exist in that directory. Therefore, only execute the script if you *really* want more data sets, for example if you are interested in testing the generalizability of the simulation results. In that case you might want to delete all of the data sets that are already contained in the directory "./datasets". 
- To reproduce the simulation (i.e., applying the anticlustering methods on the 10,000 data sets), execute all of the commands in the script "2-Call-Methods.R". This will not reproduce the results exactly because the methods rely on random number generation. Note that I did not include a random seed as the results do not depend on a particular seed.
  + I expect that running "2-Call-Methods.R" will take about 12 hours on a (not too old) personal computer. For running the simulation in smaller chunks, check out the tipps below.
- To just reproduce the analysis reported in the paper (and not re-run the simulation), execute all commands in the script "3-Compute-Objectives.R" and "4-Aggregate-Results.R". Doing so will compute the three objectives reported in the manuscript for each simulation run for each method (by writing the results to the files "results-K2-aggregated.csv", "results-K3-aggregated.csv" and "results-K4-aggregated.csv")
- The file CODEBOOK.md contains information on the variables in the data sets.
- Make sure that the R working directory is set into this directory (i.e., the directory where this README file resides) when working with the simulation R scripts.
- To reproduce the results *exactly as presented* in the paper, check out the Rmd source file "Paper.Rmd" in the upper directory.

## Dependencies: 

To reproduce the simulation itself (i.e., applying anticlustering functions to the data in the script "2-Call-Methods.R"), the `anticlust` package must be installed (version >= 0.5.6). To compute and aggregate the results, you additionally need the R packages `tidyr`, `dplyr` and `ggplot2`. For `tidyr`, I used version 1.1.3 in my simulation. I expect that future versions will produce the same results because I only relied on rather basic and stable functionality. I expect the same for `ggplot2` (version 3.3.5) and `dplyr` (version 1.0.7).

If you want to recreate the Rmd paper file, you need the same R packages and additionally the R package `papaja` (I used version 0.1.0.9997, currently need yet available froM CRAN, installable via Github from https://github.com/crsh/papaja). The Rmd source file works with the aggregated simulation results (files "results-K2-aggregated.csv", "results-K3-aggregated.csv" and "results-K4-aggregated.csv"). However, you may also extract the code from the relevant Rmd chunks if you do not want to work with R Markdown.

## Tipps

### Run the simulation piece by piece

To not run the entire simulation on all 10,000 data sets, I recommend to uncomment line 32 in the file "2-Call-Methods.R":

- `# files <- sample(files, size = 300)`

Adjust the value given to `size` as needed; it determines the number of data sets that are processed using the different anticlustering techniques (here: 300). The logic of the script ensures that no data set is processed more than once, so feel free to adjust the number of data sets and repeat the simulation as often as needed. The results will be appended to the files "results-K2-objectives-raw.csv", "results-K3-objectives-raw.csv" and "results-K4-objectives-raw.csv" whenever you call "2-Call-Methods.R". So if you call the script repeatedly, the results files will grow. If you want to restart a simulation, you should delete the files "results-K2-objectives-raw.csv", "results-K3-objectives-raw.csv" and "results-K4-objectives-raw.csv" (and also "results-K2-aggregated.csv", "results-K3-aggregated.csv" and "results-K4-aggregated.csv" if you have already computed results using the scripts "3-Compute-Objectives.R" and "4-Aggregate-Results.R"). 
