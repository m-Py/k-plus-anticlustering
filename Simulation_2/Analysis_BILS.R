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


# plot(tapply(df$time_optimal_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")
# plot(tapply(df$time_vanilla_s, santoku::chop(df$N, breaks = c(20, 40, 60, 80, 100)), median) |> round(2), xlab = "N", ylab = "Time (s)")


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
df$RESTRICTION <- "none" 
df$RESTRICTION[df$N_DUPLICATE_PARTITIONS == 99] <- "maximal"
df$RESTRICTION[df$N_DUPLICATE_PARTITIONS > 0 & df$N_DUPLICATE_PARTITIONS < 99] <- "some"
df$RESTRICTION <- ordered(df$RESTRICTION, levels = c("none", "some", "maximal"))


table(df$RESTRICTION)
table(df$RESTRICTION, df$K)

# Just group by RESTRICTION

(results_by_restriction <- df |>
    group_by(RESTRICTION) |>
    summarize(
      N = n(),
      E_1 = mean(DIV_E_1),
      E_1_ILS = mean(DIV_E_1_ILS),
      E_ALL = mean(DIV_E_ALL),
      E_ALL_ILS = mean(DIV_E_ALL_ILS),
      E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
      E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
      LCW = mean(DIV_LCW),
      LCW_ILS = mean(DIV_LCW_ILS)
    ) |> 
    as.data.frame())  # Tibble wtf you doing with decimals

results_only <- subset(results_by_restriction, select = -c(RESTRICTION, N))
results_by_restriction$Best_Performer <- colnames(results_only)[apply(results_only, 1, which.max)]
results_by_restriction[, 3:10] <- round(results_by_restriction[, 3:10], 2)
results_by_restriction

# Group by restriction and K

df |>
  group_by(K, RESTRICTION) |>
  summarize(
    E_1 = mean(DIV_E_1),
    E_1_ILS = mean(DIV_E_1_ILS),
    E_ALL = mean(DIV_E_ALL),
    E_ALL_ILS = mean(DIV_E_ALL_ILS),
    E_ALL_RESTRICTED = mean(DIV_E_ALL_RESTRICTED),
    E_ALL_RESTRICTED_ILS = mean(DIV_E_ALL_RESTRICTED_ILS),
    LCW = mean(DIV_LCW),
    LCW_ILS = mean(DIV_LCW_ILS),
    N = n()
  )

## E_ALL_ILS best for maximally restricted data sets!!

# In maximally restricted data sets, using ILS on top of MBPI or LCW improves results!
# LCW is better than MBPI, this should be part of the paper.  (at least for this restricted
# version, VANILLA is still very good)

# Primary results: Best diversity results are obtained when inducing multiple partitions that already have optimal dispersion.
# For this reason it is useful that our optimal algorithm can return multiple partitions (cf. E_ALL_ILS performing best for 
# maximally restricted problems).
# Some nuance: The more restricted the problem gets through the maximum dispersion constraint,
# the less constrained the search space should be (but restriction in general is good).
# 1. If the problem is not restricted, restricting the search space maximally is best
#    (i.e., E_ALL_RESTRICTED and LCW_RESTRICTED are best).
# 2. If the problem is somewhat restricted, restricting the search space slightly
#    is best (LCW_ILS + E_ALL_RESTRICTED_ILS are best).
# 3. If the problem is maximally restricted, E_ALL_ILS is best (LCW_ILS + E_ALL_RESTRICTED_ILS still good)
# -> This case differentiation can actually be implemented in the `anticlust` interface because
#    the degree of restriction is known!
# (depending on the input method; give a warning when using LCW with a maximally restricted data set!) 

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
    LCW_ILS = mean(DIV_LCW_ILS),
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
    LCW_ILS = mean(DIV_LCW_ILS),
    VANILLA = mean(DIV_VANILLA), # also add N per row to illustrate bias!
    N = n()
  ) |>
  as.data.frame()|>
  round(2)

table(df$N_DUPLICATE_PARTITIONS > 0, df$K) # also check out results in dependence of duplicate partitions!

prop.table(table(df$RESTRICTION, df$K, df$N > 50), margin = c(2, 3)) |> round(2) #!!!!!!
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

# TODO: Better inference statistics (regression / ANOVA)
