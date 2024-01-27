library(anticlust)
library(tidyr)
library(dplyr)
library(BayesFactor)

df <- read.csv("results_bils.csv", sep = ";")
sum(!complete.cases(df)) # =)
nrow(df)

length(unique(df$file)) # number of simulation runs / files

table(table(df$file)) # all 6!

# All E methods have optimal solution?
sum(df$DISP_E_1 != df$DISP_E_ALL) # =)
sum(df$DISP_E_1 != df$DISP_E_ALL_RESTRICTED)

# Time to solve optimally
max(df$time_optimal_s)
max(df$time_vanilla_s)
tapply(df$time_optimal_s, list(df$N, df$K), median) |> round(2)
tapply(df$time_vanilla_s, list(df$N, df$K), median) |> round(2)

table(df$RUNS_BILS_VANILLA)
table(df$RUNS_BILS_VANILLA, df$K)

# this is highly interesting: time to solve max dispersion problem optimally is related to the number of runs the heuristic requires to find optimal solution (not surprising, but nice)
tapply(df$time_optimal_s, list(df$RUNS_BILS_VANILLA), median) |> round(2)

df$VANILLA_FOUND_OPTIMUM <- df$DISP_VANILLA == df$DISP_E_1
mean(df$VANILLA_FOUND_OPTIMUM)
tapply(df$VANILLA_FOUND_OPTIMUM, df$K, mean) |> round(2)
tapply(df$VANILLA_FOUND_OPTIMUM, list(df$K, df$separate_dispersion_distances), mean) |> round(2)

# not sure if this analysis makes sense:
colMeans(df[df$VANILLA_FOUND_OPTIMUM & df$separate_dispersion_distances == 0, grepl("DIV", colnames(df))])

colMeans(df[df$VANILLA_FOUND_OPTIMUM & df$separate_dispersion_distances == 1, grepl("DIV", colnames(df))])

tt <- df[df$VANILLA_FOUND_OPTIMUM, ]

t.test(tt$DIV_VANILLA, tt$DIV_E_1, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(tt$DIV_E_1, tt$DIV_E_ALL, paired = TRUE)
t.test(tt$DIV_E_1, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(tt$DIV_E_ALL, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
# VANILLA seems to best optimize the diversity, but does not always find the optimal dispersion. so this analysis is actually flawes because of systematic missing values... what is the most appropriate analysis? BILS-E with restrictions may even be better with regard to max diversity! (even in flawed analysis with dropouts)

# Important take aways (preliminary)

# Diversity is lower if the dispersion is optimized on the basis of different data (that makes sense I guess)
# E_ALL is better than E_1 (it seems...)
# VANILLA BILS has more difficulties finding the optimal solution if the dispersion is optimized on the basis of a different data set
# The max dispersion problem can be solved in reasonable time using an open source solver (!) for rather large data sets (N = 300, K = 4)
# Restricted method may be best (?). This would indicate that using LCW is just as effective as BILS


lf <- pivot_longer(df, cols = starts_with(c("DIV", "DISP")))
lf$Method <- as.factor(gsub("DISP_|DIV_", replacement = "", x = lf$name))
lf$Objective <- as.factor(
  gsub("_E_1|_E_ALL|_VANILLA", replacement = "", 
  x = lf$name)
)

lf$K <- factor(lf$K)
lf$N <- factor(lf$N)
lf$M <- factor(lf$M)
lf$file <- factor(lf$file)

lf_div <- subset(lf, Objective == "DIV" & VANILLA_FOUND_OPTIMUM)

m1 <- lmBF(value ~ N + M + file, whichRandom = "file", data = lf_div)
m2 <- lmBF(
  value ~ N + M + Method + file, whichRandom = "file", 
  data = lf_div
)

m2/ m1

ttestBF(
  lf_div$value[lf_div$Method == "VANILLA"],
  lf_div$value[lf_div$Method == "E_ALL"],
  paired = TRUE
)
