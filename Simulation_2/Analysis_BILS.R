library(anticlust)
library(tidyr)
library(dplyr)
library(santoku)

df <- read.csv("results_bils.csv", sep = ";")
sum(!complete.cases(df)) # =)
nrow(df)

length(unique(df$file)) # number of simulation runs / files

table(table(df$file)) 
table(df$K)

# All optimal dispersion methods have optimal solution?
sum(df$DISP_E_1 != df[, grepl("DISP_E|DISP_LCW", colnames(df))]) # !

# Time to solve optimally
max(df$time_optimal_s)
max(df$time_vanilla_s)
max(df$time_lcw_s) # not entirely useful metric as it also contains solving a graph coloring problem
tapply(df$time_optimal_s, list(df$K), median) |> round(2)
tapply(df$time_vanilla_s, list(df$K), median) |> round(2)
tapply(df$time_lcw_s, list(df$K), median) |> round(2)

tapply(df$time_optimal_s, list(df$K), max) |> round(2)
tapply(df$time_vanilla_s, list(df$K), max) |> round(2)
tapply(df$time_lcw_s, list(df$K), max) |> round(2)

# plot(tapply(df$time_vanilla_s, list(df$N), median))
df$N_Category <- santoku::chop(df$N, breaks = c(20, 60, 100))

tapply(df$time_optimal_s, list(df$K, df$N_Category), median) |> round(2)


plot(tapply(df$time_optimal_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")
plot(tapply(df$time_vanilla_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")


df$VANILLA_FOUND_OPTIMUM <- df$DISP_VANILLA == df$DISP_E_1
tapply(df$VANILLA_FOUND_OPTIMUM, df$K, mean)

table(df$N_DUPLICATE_PARTITIONS)

# Display global results across all conditions
GLOBAL_RESULTS <- data.frame(Diversity = colMeans(df[, grepl("DIV_E|DIV_LCW", colnames(df))]))
GLOBAL_RESULTS[order(GLOBAL_RESULTS$Diversity), , drop = FALSE] |> round(2)

# (TODO: I really need the data in long format for better descriptives...)

# There seems to be a clear order in the results: 3 groups of performers: 
# E_1 and E_1_ILS are worst (E_1 is particularly bad for some reason)
# Then the non-ILS variants come: E_ALL, E_ALL_RESTRICTED, LCW
# Then the ILS variants come: E_ALL_ILS, E_ALL_RESTRICTED_ILS, LCW_ILS

# Descriptives without VANILLA (unbiased)
df |>
  group_by(K) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_1_ILS = mean(DIV_E_1_ILS),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_ILS = mean(DIV_E_ALL_ILS),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
    LCW = mean(DIV_LCW),
    LCW_ALT_ILS = mean(DIV_LCW_ILS),
    N = n()
  ) |>
  as.data.frame() |>
  round(2)

# What happens in maximally restricted data sets?

# What happens in maximally restricted data sets? (with VANILLA, biased)
df$MAXIMUM_RESTRICTION <- as.numeric(df$N_DUPLICATE_PARTITIONS == 99)

table(df$MAXIMUM_RESTRICTION)
table(df$MAXIMUM_RESTRICTION, df$K)

df |>
  group_by(K, MAXIMUM_RESTRICTION) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_1_ILS = mean(DIV_E_1_ILS),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_ILS = mean(DIV_E_ALL_ILS),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
    LCW = mean(DIV_LCW),
    LCW_ALT_ILS = mean(DIV_LCW_ILS),
    N = n()
  ) |>
  as.data.frame() |>
  round(2)

# In maximally restricted data sets, using ILS on top of MBPI or LCW improves results!
# LCW is better than MBPI, this should be part of the paper.  (at least for this restricted
# version, VANILLA is still very good)

df |>
  filter(VANILLA_FOUND_OPTIMUM == 1) |> 
  group_by(K, MAXIMUM_RESTRICTION) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_1_ILS = mean(DIV_E_1_ILS),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_ILS = mean(DIV_E_ALL_ILS),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
    LCW = mean(DIV_LCW),
    LCW_ALT_ILS = mean(DIV_LCW_ILS),
    VANILLA = mean(DIV_VANILLA),
    N = n()
  ) |>
  as.data.frame() |>
  round(2)


# Descriptives with  VANILLA (biased!)
df |>
  filter(VANILLA_FOUND_OPTIMUM == 1) |> # this actually introduces a bias
  group_by(K) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_1_ILS = mean(DIV_E_1_ILS),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_ILS = mean(DIV_E_ALL_ILS),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
    LCW = mean(DIV_LCW),
    LCW_ALT_ILS = mean(DIV_LCW_ILS),
    VANILLA = mean(DIV_VANILLA), # also add N per row to illustrate bias!
    N = n()
  ) |>
  as.data.frame()|>
  round(2)

table(df$N_DUPLICATE_PARTITIONS > 0, df$K) # also check out results in dependence of duplicate partitions!

prop.table(table(df$MAXIMUM_RESTRICTION, df$K, df$N > 50), margin = c(2, 3)) |> round(2) #!!!!!!
# probability of duplicates increases with K and decreases with N (small group sizes lead to duplicates)

# if there is only one unique input partition, VANILLA is better than RESTRICTED,
# but otherwise not!
  

tt <- df[df$VANILLA_FOUND_OPTIMUM, ] # slight bias in favor of VANILLA, in particular for K = 6, K = 7

t.test(tt$DIV_VANILLA, tt$DIV_E_1, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL, paired = TRUE)
t.test(tt$DIV_VANILLA, tt$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(df$DIV_E_1, df$DIV_E_ALL, paired = TRUE)
t.test(df$DIV_E_1, df$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(df$DIV_E_ALL, df$DIV_E_ALL_RESTRICTED, paired = TRUE)
t.test(df$DIV_E_ALL_RESTRICTED, df$DIV_E_ALL_RESTRICTED_ALT, paired = TRUE)
t.test(df$DIV_E_ALL_RESTRICTED_ALT, df$DIV_LCW, paired = TRUE)

# Important take aways (preliminary)

# - E_ALL is better than E_1; E_ALL_RESTRICTED is usually best extension 
# - VANILLA BILS has more difficulties finding the optimal diversity if the dispersion is optimized on the basis of a different data set
# - Diversity in general is lower if the dispersion is optimized on the basis of different data (that makes sense I guess)
# - The max dispersion problem can be solved in reasonable time using an open source solver (!) for rather large data sets and K
# - Restricted method is the best extension that ensures the maximum dispersion (This would indicate that using unicriterion LCW is just as effective as BILS)
# - Interpretation: Best for optimizing diversity while maintaining the global optimum in dispersion: restricted version if there are many init partitions that have the optimal value in dispersion. Worst: Only pass 1 init partition. Vanilla is usually good, but does not necessarily find global optimum (this can be checked using the optimal_dispersion() function). If the global optimum is needed, use restricted version; if there are no duplicates: stick with it.
# - RESTRICTED METHOD IS NOT GOOD IF THE NUMBER OF UNIQUE PARTITIONS IS SMALL 
# - Duplicate partitions more likely arise for smaller group sizes (i.e., larger K, and smaller N)
