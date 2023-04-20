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
names_best_method <- sapply(results, function(x) names(which.max(x[, 5])))
unstd_best <- names_best_method == "unweighted_std"
sum(unstd_best)
mean(unstd_best)

# Significance test
frequency_table <- c(table(names_best_method), rep(0, 6 - length(table(names_best_method))))
chisq.test(frequency_table)

# Or simply:
prop.test(x = sum(unstd_best), n = length(unstd_best), p = 1/6)
