
# README

- Author: Martin Papenberg

- Year: 2022

---

This directory contains code and data to reproduce and analyze the simulation study reported in: 

"k-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity" (Papenberg, 2022).  **XXX TODO**

## General information

- The directory "./datasets" contains the 10,000 data sets used in the simulation
- R scripts that start with "0-functions" contain function definitions that are used throughout the simulation. These files are "sourced" from the R-scripts that start with numbers >= 1 ("1-Generate-Data.R"; "2-Call-Methods.R"; "3-Compute-Objectives.R"; "4-Aggregate-Results.R")
- The script "1-Generate-Data.R" should not be called directly because doing so would generate another 10,000 data files in the directory "./datasets", but the data sets that were used in the paper already exist in that directory. Therefore, only execute the script if you *really* want more data sets, for example if you are interested in testing the generalizability of the simulation results. In that case you might want to first delete all of the data sets that are already contained in the directory "./datasets". 
- To reproduce the simulation (i.e., applying the anticlustering methods on the 10,000 data sets), execute all of the commands in the script "2-Call-Methods.R". This will not reproduce the results exactly because the methods rely on random number generation. I did not include a fixed random seed as the results do not depend on a particular seed. Then calling "3-Compute-Objectives.R" will compute the results based on the anticlustering assignments. This step does not use random number generation.
  + Running "2-Call-Methods.R" will write the data sets "results-K2-solutions.csv", "results-K3-solutions.csv" and "results-K4-solutions.csv".
  + Running "3-Compute-Objectives.R" will write the files "results-K3-objectives-raw.csv", "results-K2-objectives-raw.csv" and "results-K4-objectives-raw.csv"
  + Running both simulation script probably takes about 1,5 days on a personal computer
- If you want to restart a simulation, you should first delete the files "results-K2-objectives-raw.csv", "results-K3-objectives-raw.csv" and "results-K4-objectives-raw.csv" (and also "results-K2-solutions.csv", "results-K3-solutions.csv" and "results-K4-solutions.csv" if you have already computed results using the script "3-Compute-Objectives.R").
- To just reproduce the analysis reported in the paper (and not re-run the simulation), run the script "4-Aggregate-Results.R". To reproduce all results / graphs *exactly as presented* in the paper, check out the Rmd source file "Paper.Rmd" in the upper directory.
- The file CODEBOOK.md contains information on the variables in the data sets "results-..-objectives-raw.csv" and "results-..-solutions.csv"
- Make sure that the R working directory is set into this directory (i.e., the directory where this README file resides) when working with the simulation R scripts.

## Dependencies: 

To reproduce the simulation itself (i.e., applying anticlustering functions to the data in the script "2-Call-Methods.R"), the `anticlust` package must be installed (version >= 0.6.1). To compute and aggregate the results, you additionally need the R packages `tidyr`, `dplyr`, `ggplot2` and `DescTools`. For `tidyr`, I used version 1.1.3 in my simulation; for `ggplot2` version 3.3.5, for `dplyr` version 1.0.7 and for `DescTools` 0.99.45. I expect that future versions will produce the same results because I only relied on rather basic and stable functionality.

If you want to recreate the R Markdown paper file ("Paper.Rmd" in the upper directory), you need the same R packages and additionally the R package `papaja` (I used version 0.1.0.9997, currently need yet available from CRAN, installable via Github from https://github.com/crsh/papaja). However, you may also extract the code from the relevant Rmd chunks if you do not want to work with R Markdown.

## Tipps

### Run the simulation piece by piece

To not run the entire simulation on all 10,000 data sets, I recommend to uncomment line 32 in the file "2-Call-Methods.R":

- `# files <- sample(files, size = 300)`

Adjust the value given to `size` as needed; it determines the number of data sets that are processed using the different anticlustering techniques (here: 300). The logic of the script ensures that no data set is processed more than once, so feel free to adjust the number of data sets and repeat the simulation as often as needed. The results will be appended to the files "results-K2-solutions.csv", "results-K3-solutions.csv" and "results-K4-solutions.csv" whenever you call "2-Call-Methods.R". So if you call the script repeatedly, the results files will grow. 
