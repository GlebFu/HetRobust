source(file = "Long Ervin Replication.R")

set.seed(20150707)

design <- list(n = c(25, 50, 100, 250, 500, 1000),
               B = "1 1 1 1 0 0",
               Estruct = c("E0", "E1", "E2", "E3", "E4", "E5", "E6"),
               whichX = "T T T T T F",
               Edist = c("En", "Ech", "Et"),
               adjust = "HOM HC0 HC1 HC2 HC3 HC4 HC4m HC5 HC2_fp HC3_fp HC4_fp HC4m_fp HC5_fp")

params <- expand.grid(design, stringsAsFactors = F)

params$iterations <- 5
params$seed <- round(runif(nrow(params)) * 2^30)

source_obj <- ls()
cluster <- start_parallel(source_obj)

system.time(results <- mdply(params, .fun = runSim, .parallel = T))

stopCluster(cluster)

write.csv(results, file = "Results/20150707.csv")