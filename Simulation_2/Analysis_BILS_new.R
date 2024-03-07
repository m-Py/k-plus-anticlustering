
library(tidyr)
library(dplyr)
library(santoku)

df <- read.csv("results_bils.csv", sep = ";")

# Two functions for printing descriptive results

# Function to add the difference between best and worst method
# primarily shows in which conditions E_1 is really bad
add_discrepancy <- function(results, colpattern = "DIV") {
  results_only <- results[, grepl(colpattern, colnames(results))]
  MIN <- apply(results_only, 1, min)
  MAX <- apply(results_only, 1, max)
  results$Discrepancy <- MAX - MIN
  results
}

add_best_method <- function(results, colpattern = "DIV") {
  results_only <- results[, grepl(colpattern, colnames(results))]
  results$Best_Performer <- colnames(results_only)[apply(results_only, 1, which.max)]
  results
}

df$VANILLA_FOUND_OPTIMUM <- df$DISP_VANILLA == df$DISP_E_1
df$RESTRICTION <- "none" 
df$RESTRICTION[df$N_DUPLICATE_PARTITIONS == 99] <- "maximal"
df$RESTRICTION[df$N_DUPLICATE_PARTITIONS > 0 & df$N_DUPLICATE_PARTITIONS < 99] <- "some"
df$RESTRICTION <- ordered(df$RESTRICTION, levels = c("none", "some", "maximal"))

sort(colMeans(df[df$VANILLA_FOUND_OPTIMUM == 1, grepl("DIV", colnames(df))])) # global results including VANILLA
# THE ALL_ILS adaptations perform better than VANILLA even when VANILLA found optimum (which is a biased analysis!):
t.test(
  df$DIV_VANILLA[df$VANILLA_FOUND_OPTIMUM == 1], 
  df$DIV_E_ALL_ILS[df$VANILLA_FOUND_OPTIMUM == 1], 
  paired = TRUE
)
t.test(
  df$DIV_VANILLA[df$VANILLA_FOUND_OPTIMUM == 1], 
  df$DIV_E_ALL_RESTRICTED_ILS[df$VANILLA_FOUND_OPTIMUM == 1], 
  paired = TRUE
)
t.test(
  df$DIV_VANILLA[df$VANILLA_FOUND_OPTIMUM == 1], 
  df$DIV_LCW_ILS[df$VANILLA_FOUND_OPTIMUM == 1], 
  paired = TRUE
)

# Did Vanilla find optimum? Depending on Restriction
prop.table(table(df$RESTRICTION, c("NO OPTIMUM", "FOUND OPTIMUM")[df$VANILLA_FOUND_OPTIMUM+1]), margin = 2) |> round(2)

dfw <- select(df, file, N, M, K, RESTRICTION, starts_with("DIV"), -DIV_VANILLA)

dfw$DIV_MAX <- apply(dfw[, grepl("DIV", colnames(dfw))], 1, max)

almost_has_maximum_div <- ((dfw[, grepl("DIV", colnames(dfw))] / dfw$DIV_MAX) > .999) * 1 # maybe good quantity?
almost_has_maximum_div <- almost_has_maximum_div[, colnames(almost_has_maximum_div) != "DIV_MAX"]
colnames(almost_has_maximum_div) <- paste0("MAX_", colnames(almost_has_maximum_div))
dfw <- data.frame(dfw, almost_has_maximum_div)

sort(colMeans(almost_has_maximum_div))

dfw_percent <- select(dfw, file, N, M, K, RESTRICTION, starts_with("MAX"))
dflP <- pivot_longer(
  dfw_percent, 
  cols = starts_with("MAX"), 
  names_to = "Method", 
  values_to = "Max_Diversity", names_prefix = "MAX_")

dfw_diversity <- select(df, file, N, M, K, RESTRICTION, starts_with("DIV"), -DIV_VANILLA)
dflD <- pivot_longer(
  dfw_diversity, 
  cols = starts_with("DIV"), 
  names_to = "Method", 
  values_to = "Diversity")

# Descriptive statistics:

# Global: 
dflP |> 
  group_by(Method) |> 
  summarize(Percent = mean(Max_Diversity), n = n()) |> 
  arrange(-Percent)

dflD |> 
  group_by(Method) |> 
  summarize(Diversity = mean(Diversity), n = n()) |> 
  arrange(-Diversity)

# Grouped by K:
dflP |> 
  group_by(K, Method) |> 
  summarize(Percent = mean(Max_Diversity), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Percent) |> 
  add_best_method()

dflD |> 
  group_by(K, Method) |> 
  summarize(Diversity = mean(Diversity), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Diversity) |> 
  add_best_method()

# Grouped by Restriction:
dflP |> 
  group_by(RESTRICTION, Method) |> 
  summarize(Percent = round(mean(Max_Diversity), 3), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Percent) |> 
  add_best_method()

dflD |> 
  group_by(RESTRICTION, Method) |> 
  summarize(Diversity = round(mean(Diversity), 3), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Diversity) |> 
  add_best_method()

# I guess these two tables tell most of the story: 
# Grouped by Restriction and N:
dflP |> 
  group_by(RESTRICTION, Method, N_category = santoku::chop(N, breaks = c(20, 60, 100))) |> 
  summarize(Percent = round(mean(Max_Diversity), 3), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Percent) |> 
  add_discrepancy() |> 
  add_best_method()

dflD |> 
  group_by(RESTRICTION, Method, N_category = santoku::chop(N, breaks = c(20, 60, 100))) |> 
  summarize(Diversity = round(mean(Diversity), 3), n = n()) |> 
  pivot_wider(names_from = Method, values_from = Diversity) |> 
  add_discrepancy() |> 
  add_best_method()

# E_1 may actually be good for small data sets + maximum restriction
# (but I would still prefer a different method due to the danger of "serious fails")

# E_1 becomes increasingly bad with increasing N, especially for restricted data sets

# What is going on with E_1:
differences <- df$DIV_E_1[df$RESTRICTION == "maximal"] - df$DIV_E_ALL_RESTRICTED[df$RESTRICTION == "maximal"]
plot(sort(differences))
mean(differences > 0) # most of the time E_1 is better, but sometimes it really fails...
mean(differences)

# I guess this analysis should be included in the paper.
