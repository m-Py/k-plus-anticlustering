
# README

- Author: Martin Papenberg
- Year: 2023

---

This directory contains code to reproduce and analyze Simulation 1 reported in: 

"k-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity" (Papenberg, 2023).

- The file `1_weight_simulation.R` reproduces the simulation, a seed is included for exact reproducibilty. Remove the seed if you want to inspect the results for different data sets. 
- Executing the script `1_weight_simulation.R` will write the simulation results to an `Rdata` file, which can be loaded in `R` using `get(load(...))` and contains a `list` of the simulation results. 
- Check out the Rmd file of the manuscript or `2_analysis.R` to reproduce the analysis reported in the paper.
- Functions are used that are defined in `0_functions_weight_simulation.R`, check out that file for a better understanding of the process. 
- The simulation for N = 100 data sets took 14 hours on my personal computer. 



