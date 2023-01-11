
# Author: Martin Papenberg
# Year: 2023

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
  cols = paste0(c("means", "sd", "skew", "kur", "cor"), "_obj"),
  names_to = "Objective",
  names_pattern = "(.*)_obj"
)

## Global results, aggregated across all simulation variables
ldf %>% 
  group_by(method, Objective) %>% 
  summarise(Mean = round(mean(value), 2)) %>% 
  pivot_wider(names_from = Objective, values_from = Mean) %>% 
  select(c(means, sd, skew, kur, cor))


# Plot the results, by N
ldf %>% 
  group_by(method, Objective, N) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(method != "random") %>% 
  ggplot(aes(x = N, y = Mean, colour = method)) + 
  geom_line(size = 1) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  theme_bw(base_size = 22)


### For the appendix -- other splits for the main results as plot (N is most important)
# Plot the results, by M
ldf %>% 
  group_by(method, Objective, M) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(method != "random") %>% 
  mutate(
    Objective = ordered(
      Objective, 
      levels = c("means", "sd", "skew", "kur", "cor"),
      labels = c("M", "SD", "Skew", "Kurtosis", "Correlation"))
  ) %>% 
  ggplot(aes(x = M, y = Mean, colour = method)) + 
  geom_line(aes(linetype = method), size = .85) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  ylab("Mean discrepancy") +
  theme_bw(base_size = 16)

# Plot the results, by SD
ldf %>% 
  group_by(method, Objective, SD) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(method != "random") %>% 
  mutate(
    Objective = ordered(
      Objective, 
      levels = c("means", "sd", "skew", "kur", "cor"),
      labels = c("M", "SD", "Skew", "Kurtosis", "Correlation"))
  ) %>% 
  ggplot(aes(x = SD, y = Mean, colour = method)) + 
  geom_line(aes(linetype = method), size = .85) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  ylab("Mean discrepancy") +
  theme_bw(base_size = 16)

# Plot the results, by K
ldf %>% 
  group_by(method, Objective, K) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(method != "random") %>% 
  mutate(
    Objective = ordered(
      Objective, 
      levels = c("means", "sd", "skew", "kur", "cor"),
      labels = c("M", "SD", "Skew", "Kurtosis", "Correlation"))
  ) %>% 
  ggplot(aes(x = K, y = Mean, colour = method)) + 
  geom_line(aes(linetype = method), size = .85) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  ylab("Mean discrepancy") +
  theme_bw(base_size = 16)

# Plot the results, by r
ldf %>% 
  group_by(method, Objective, r) %>% 
  summarise(Mean = mean(value)) %>% 
  filter(method != "random") %>% 
  mutate(
    Objective = ordered(
      Objective, 
      levels = c("means", "sd", "skew", "kur", "cor"),
      labels = c("M", "SD", "Skew", "Kurtosis", "Correlation"))
  ) %>% 
  ggplot(aes(x = r, y = Mean, colour = method)) + 
  geom_line(aes(linetype = method), size = .85) + 
  facet_grid(rows = vars(Objective), scales = "free") + 
  ylab("Mean discrepancy") +
  theme_bw(base_size = 16)

# Check number of simulations per condition
ldf %>% 
  group_by(method, Objective, K) %>% 
  summarise(N = n()) %>% 
  pull(N)
