
# Codebook

- Author: Martin Papenberg
- Year: 2023

---

This file is the codebook for the data sets 

- results-K2-solutions.csv
- results-K3-solutions.csv
- results-K4-solutions.csv
- results-K2-objectives-raw.csv
- results-K3-objectives-raw.csv
- results-K4-objectives-raw.csv

These files contain the simulation data reported in the manuscript "k-plus Anticlustering: An Improved k-means Criterion for Maximizing Between-Group Similarity" (Papenberg, 2023).

--- 

## General information

The data sets are given in a format where each row is uniquely represented by a combination of (a) (anticlustering) method and (b) an ID that uniquely identifies a data set to which the (anticlustering) methods were applied. 10,000 data sets were processed. The "solutions" files and the "objectives-raw" files contain different information on the same simulation runs. Each data set was processed using K = 2, 3 and 4. K varies between the csv files and is constant within a file (as shown by the file naming).

## Columns

The data sets store the following variables:

- file: the file name of the simulation data set that was processed
- ID: A randomly generated ID that uniquely identifies a data set 
- N: The sample size associated with a simulation data set
- M: The number of features associated with a simulation data set
- SD: The standard deviation of the normal distribution that generated the data set
- r: The correlation between features in the normal distribution that generated the data set
- method: The method that was applied to the data set
    + "diversity" anticlustering,
    + "k-means" anticlustering,
    + "k-plus" anticlustering,
    + "k-plus-skew-kurtosis" anticlustering,
    + "k-plus-correlation" anticlustering,
    + "random" allocation
- result: A string representing the solution that a method returned (i.e., how the elements were assigned to groups)
- means_obj: A measure of the discrepancy in the feature means (lower = better)
- sd_obj: A measure of the discrepancy in the feature standard deviations (lower = better)
- skew_obj: A measure of the discrepancy in skewness (lower = better)
- kur_obj: A measure of the discrepancy in kurtosis (lower = better)
- cor_obj: A measure of the discrepancy in correlations (lower = better)

For a thorough description of the computed objectives and the anticlustering methods, see the manuscript. 
