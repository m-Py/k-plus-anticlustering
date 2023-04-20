# Read simulation results
# (in case there are several results files)
files <- list.files(".", pattern = ".Rdata")
results <- list()
for (file in files) {
  results <- c(results, get(load(file)))
}

# Table reported in the manuscript: 
round(Reduce("+", results) / length(results) * 100, 2)

# how often was unweighted standardization approach best:
unstd_best <- sapply(results, function(x) names(which.max(x[, 5]))) == "unweighted_std"
sum(unstd_best)
mean(unstd_best)

# Significance test
prop.test(x = sum(unstd_best), n = length(unstd_best), p = 1/6)
