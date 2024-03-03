# ANALYSIS_BILS_VANILLA.R


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


# SPLIT BY M
prop.table(table(df$RUNS_BILS_VANILLA, df$M), margin = 2) |> round(2) # also strongly affects the performance, use larger M in simulation?

## ALSO SPLIT BY N; BUT USE CATEGORIES
df$N_Category <- santoku::chop(df$N, breaks = c(20, 60, 100))
prop.table(table(df$RUNS_BILS_VANILLA, df$N_Category), margin = 2) |> round(3) * 100


# this is highly interesting: time to solve max dispersion problem optimally is related to the number of runs the heuristic requires to find optimal solution (not surprising, but nice)
tapply(df$time_optimal_s, list(df$RUNS_BILS_VANILLA), median) |> round(2)


