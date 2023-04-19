


source("./Simulation_Study/Weight_Simulation/functions_weight_simulation.R")

nsim <- 10
results <- list()
for (i in 1:nsim) {
  K <- sample(2:4, size = 1)
  N <- sample((10:100)[10:100 %% K == 0], size = 1)
  M <- sample(1:5, size = 1)
  message("Starting to work on N = ", N, ", K = ", K, ", M = ", M, ".")
  dat <- matrix(rnorm(N * M), ncol = M)
  start <- Sys.time()
  results[[i]] <- weight_sim(dat, K = K)
  print(Sys.time() - start)
  results[[i]] <- cbind(results[[i]], rowMeans(results[[i]]))
  colnames(results[[i]]) <- c("SSE", "SSE_Var", "SSE_Skew", "SSE_Kurtosis", "Average")
}

save(results, file = paste0("./Simulation_Study/Weight_Simulation/Results Sim 1 ", date(), ".Rdata"))
