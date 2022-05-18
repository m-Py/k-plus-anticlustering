
# Author: Martin Papenberg
# Year: 2022

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

## Analyze data for K = 2 and K = 3 and K = 4
simulation_results <- list()
for (K in 2:4) {
  filename <- paste0("results-K", K, "-objectives-raw.csv")
  df <- read.csv(filename, sep = ";", stringsAsFactors = FALSE)
  df$K <- K
  simulation_results[[paste0("K-", K)]] <- df
}

df <- do.call(rbind, simulation_results)
rownames(df) <- NULL

length(unique(df$ID))
table(table(df$ID))

# Make long format
ldf <- pivot_longer(
  df,
  cols = paste0(c("kvar", "kmeans", "means", "sd", "skew", "kur", "cor"), "_obj"),
  names_to = "Objective",
  names_pattern = "(.*)_obj"
)

# Plot the results
ldf %>% 
  group_by(method, Objective, N) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(Objective %in% c("means", "sd", "skew", "kur", "cor"), method != "random") %>% 
  ggplot(aes(x = N, y = Mean, colour = method)) + 
  geom_line(size = 1) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  theme_bw(base_size = 22)

## by value, aggregate across N and K
ldf %>% 
  group_by(method, Objective) %>% 
  summarise(Mean = round(mean(value), 3)) %>% 
  filter(Objective %in% c("means", "sd", "skew", "kur", "cor"), method != "random") %>% 
  arrange(Objective) %>% View()

# Check number of simulations per condition
ldf %>% 
  group_by(method, Objective, K) %>% 
  summarise(N = n()) %>% 
  pull(N)
