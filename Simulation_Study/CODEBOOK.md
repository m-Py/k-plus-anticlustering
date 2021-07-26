
# Codebook

Author: Martin Papenberg

Year: 2021

---

This file is the codebook for the data sets 

- results-K2-solutions.csv
- results-K3-solutions.csv
- results-K4-solutions.csv
- results-K2-objectives-raw.csv
- results-K3-objectives-raw.csv
- results-K4-objectives-raw.csv

These files contain the simulation data reported in the manuscript "k-plus: a bicriterion extension of k-means anticlustering" (Papenberg, 2021). **XXX TODO**

--- 

## General information

The data sets are given in a format where each row is uniquely represented by a combination of (a) anticlustering method and (b) an ID that uniquely identifies a data set to which anticlustering methods were applied. The "solutions" files and the "objectives-raw" files contain different information on the same simulation runs (identified by the unique IDs). Each data set was processed using K = 2, 3 and 4. Each of the methods "k-means anticlustering", "k-plus anticlustering" and "anticluster editing" was applied to each data set (for K = 2, 3 and 4 respectively). K varies between the csv files and is constant within a file (as shown by the file naming).

## Columns

The data sets store the following variables:

- file: the file name of the simulation data set that was processed
- ID: A randomly generated ID that uniquely identifies a data set 
- N: The sample size associated with a simulation data set
- M: The number of features associated with a simulation data set
- DIST: The distribution that generated the simulation data set
- method: The method that was applied to divide the data set
    + "ace-exchange" - anticluster editing objective (»diversity«), optimized using a local maximum search exchange method, 
    + "k-means-exchange" - k-means anticlustering objective, optimized using a local maximum search exchange method
    + "k-plus" - k-plus anticlustering objective, optimized using a local maximum search exchange method
    + "random" - A random allocation of items to groups (not presented in the paper; may be analyzed if you are curious)
- result: A string representing the solution that a method returned (how to assign the elements to sets)
- kmeans_obj: The k-means objective (higher values = better)
- kvar_obj: The k-variance objective (higher values = better; not presented in the paper; may be analyzed if you are curious)
- means_obj: A measure of the discrepancy in the feature means (lower = better)
- sd_obj: A measure of the discrepancy in the feature standard deviations (lower = better)
