library(anticlust)
library(tidyr)
library(dplyr)
library(santoku)

df <- read.csv("results_bils.csv", sep = ";")
sum(!complete.cases(df)) # =)
nrow(df)

length(unique(df$file)) # number of simulation runs / files

table(table(df$file)) # all 2!

# All E methods have optimal solution?
sum(df$DISP_E_1 != df$DISP_E_ALL) # =)
sum(df$DISP_E_1 != df$DISP_E_ALL_RESTRICTED)

# Time to solve optimally
max(df$time_optimal_s)
max(df$time_vanilla_s)
tapply(df$time_optimal_s, list(df$K), median) |> round(2)
tapply(df$time_vanilla_s, list(df$K), median) |> round(2)

tapply(df$time_optimal_s, list(df$K), max) |> round(2)
tapply(df$time_vanilla_s, list(df$K), max) |> round(2)

# plot(tapply(df$time_vanilla_s, list(df$N), median))

table(df$RUNS_BILS_VANILLA)
table(df$RUNS_BILS_VANILLA, df$K)

runs_vanilla <- prop.table(table(df$RUNS_BILS_VANILLA, df$K), margin = 2) |> round(3) * 100
runs_vanilla <- rbind(runs_vanilla, runs_vanilla[1, ])
runs_vanilla <- runs_vanilla[-1, ]
rownames(runs_vanilla)[nrow(runs_vanilla)] <- "-1"
store <- rownames(runs_vanilla)
runs_vanilla <- apply(runs_vanilla, 2, prmisc::force_decimals, 1)
rownames(runs_vanilla) <- store
runs_vanilla_formatted <- t(apply(runs_vanilla, 1, paste0, "%"))
runs_vanilla_formatted <- formatC(runs_vanilla_formatted, width = 4)
colnames(runs_vanilla_formatted) <- 2:7
runs_vanilla_formatted

## ALSO SPLIT BY N; BUT USE CATEGORIES
df$N_Category <- santoku::chop(df$N, breaks = c(20, 60, 100))
prop.table(table(df$RUNS_BILS_VANILLA, df$N_Category), margin = 2) |> round(3) * 100


plot(tapply(df$time_optimal_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")
plot(tapply(df$time_vanilla_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")


# this is highly interesting: time to solve max dispersion problem optimally is related to the number of runs the heuristic requires to find optimal solution (not surprising, but nice)
tapply(df$time_optimal_s, list(df$RUNS_BILS_VANILLA), median) |> round(2)

df$VANILLA_FOUND_OPTIMUM <- df$RUNS_BILS_VANILLA != -1
tapply(df$VANILLA_FOUND_OPTIMUM, df$K, mean)

mean(df$N_DUPLICATE_PARTITIONS == (df$RUNS_BILS_VANILLA - 1)) # !!!!

df$VANILLA_FOUND_OPTIMUM_AFTER_5 <- df$RUNS_BILS_VANILLA == 5
mean(df$VANILLA_FOUND_OPTIMUM_AFTER_5)
tapply(df$VANILLA_FOUND_OPTIMUM_AFTER_5, df$K, mean) |> round(2)
tapply(df$VANILLA_FOUND_OPTIMUM_AFTER_5, list(df$K, df$separate_dispersion_distances), mean) |> round(2)

chisq.test(table(df$VANILLA_FOUND_OPTIMUM_AFTER_5, df$separate_dispersion_distances))

# Descriptives:
df |>
  filter(VANILLA_FOUND_OPTIMUM == 1) |> # this actually introduces a bias
  group_by(K) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    VANILLA = mean(DIV_VANILLA), # also add N per row to illustrate bias!
    N = n()
  ) |>
  as.data.frame()

table(df$N_DUPLICATE_PARTITIONS > 0, df$K) # also check out results in dependence of duplicate partitions!

df$MAXIMUM_RESTRICTION <- as.numeric(df$N_DUPLICATE_PARTITIONS == (df$RUNS_BILS_VANILLA - 1))

df |>
  filter(VANILLA_FOUND_OPTIMUM == 1) |> # this actually introduces a bias
  group_by(K, MAXIMUM_RESTRICTION) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    VANILLA = mean(DIV_VANILLA),
    N = n()
  ) |>
  as.data.frame() |>
  round(2)

prop.table(table(df$MAXIMUM_RESTRICTION, df$K, df$N > 50), margin = c(2, 3)) |> round(2) #!!!!!!
# probability of duplicates increases with K and decreases with N (small group sizes lead to duplicates)

# if there is only one unique input partition, VANILLA is better than RESTRICTED,
# but otherwise not!
  
# Descriptives without bias, only Extended versions
df |>
  group_by(K) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED)
  )

tt <- df[df$VANILLA_FOUND_OPTIMUM, ] # slight bias in favor of VANILLA, in particular for K = 6, K = 7

t.test(tt$DIV_VANILLA, tt$DIV_E_1, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(tt$DIV_E_1, tt$DIV_E_ALL, paired = TRUE)
t.test(tt$DIV_E_1, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(tt$DIV_E_ALL, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)

# Important take aways (preliminary)

# - E_ALL is better than E_1; E_ALL_RESTRICTED is usually best extension 
# - VANILLA BILS has more difficulties finding the optimal diversity if the dispersion is optimized on the basis of a different data set
# - Diversity in general is lower if the dispersion is optimized on the basis of different data (that makes sense I guess)
# - The max dispersion problem can be solved in reasonable time using an open source solver (!) for rather large data sets and K
# - Restricted method is the best extension that ensures the maximum dispersion (This would indicate that using unicriterion LCW is just as effective as BILS)
# - Interpretation: Best for optimizing diversity while maintaining the global optimum in dispersion: restricted version if there are many init partitions that have the optimal value in dispersion. Worst: Only pass 1 init partition. Vanilla is usually good, but does not necessarily find global optimum (this can be checked using the optimal_dispersion() function). If the global optimum is needed, use restricted version.
# - RESTRICTED METHOD IS NOT GOOD IF THE NUMBER OF UNIQUE PARTITIONS IS SMALL 
# - Duplicate partitions more likely arise for smaller group sizes (i.e., larger K, and smaller N)
