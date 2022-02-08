library(tidyverse)
library(parallel)
library(feather)

#Save root working directory
root_dir <- getwd()

S_Method <- c("SRS", "Stratified", "Cluster")

#Code for unpacking sampled RDS files and writing them as feather
for (j in c(1:3)){
  for (i in c(1:10)){
    if (i < 10){
    #RDS data samples should be stored in a folder "RDS"!!
    setwd(sprintf(root_dir, "/ML Algo/RDS"))
    base <- readRDS(paste0("0", i, "_sample.rds"))[[j]]$Base
    importance <- readRDS(paste0("0", i, "_sample.rds"))[[j]]$importance_sampling
    synthetic <- readRDS(paste0("0", i, "_sample.rds"))[[j]]$synthetic_sampling

    # 2. Scale data
    base = base %>% relocate(Person_Income) # move Y column to first position
    importance = importance %>% relocate(Person_Income)
    synthetic = synthetic %>% relocate(Person_Income)

    #Create Feather Directory under ML Algo!!
    setwd(sprintf(root_dir, "/ML Algo/Feather"))
    write_feather(base, paste0("0", i, "_", s_Method[j],"_base_sample.feather"))
    write_feather(importance, paste0("0", i, "_", S_Method[j], "_importance_sample.feather"))
    write_feather(synthetic, paste0("0", i, "_", S_Method[j], "_synthetic_sample.feather"))
    }
  
    #due to indexing, if statement is required (instead of 010 we have 10 in this loop)
    if (i >= 10){
      setwd(sprintf(root_dir, "/ML Algo/RDS"))
      base <- readRDS(paste0(i, "_sample.rds"))[[j]]$Base
      importance <- readRDS(paste0(i, "_sample.rds"))[[j]]$importance_sampling
      synthetic <- readRDS(paste0(i, "_sample.rds"))[[j]]$synthetic_sampling

      base = base %>% relocate(Person_Income) 
      importance = importance %>% relocate(Person_Income)
      synthetic = synthetic %>% relocate(Person_Income)

      setwd(sprintf(root_dir, "/ML Algo/Feather"))
      write_feather(base, paste0("0", i, "_", s_Method[j],"_base_sample.feather"))
      write_feather(importance, paste0("0", i, "_", S_Method[j], "_importance_sample.feather"))
      write_feather(synthetic, paste0("0", i, "_", S_Method[j], "_synthetic_sample.feather"))
    }
  }
 }

