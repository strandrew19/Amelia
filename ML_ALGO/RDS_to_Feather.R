library(tidyverse)
library(parallel)
library(feather)

#Save root working directory
root_dir <- getwd()

#Code for unpacking sampled RDS files and writing them as feather
for (i in c(1:10)){
  if (i < 10){
  #RDS data samples should be stored in a folder "RDS"!!
  setwd(sprintf(root_dir, "/ML Algo/RDS"))
  srs <- readRDS(paste0("0", i, "_sample.rds"))$SRS$Base
  importance <- readRDS(paste0("0", i, "_sample.rds"))$SRS$importance_sampling
  synthetic <- readRDS(paste0("0", i, "_sample.rds"))$SRS$synthetic_sampling
  
  # 1. Convert to numeric
  srs$Sex = as.numeric(as.factor(unique(srs$Sex)))
  
  #This code cannot be implemented because of an internal R issue (Remedied in Python)
  #importance$Sex = as.numeric(as.factor(unique(importance$Sex)))
  #synthetic$Sex = as.numeric(as.factor(unique(synthetic$Sex)))
  
  # 2. Scale data
  srs = srs %>% relocate(Person_Income) # move Y column to first position
  importance = importance %>% relocate(Person_Income)
  synthetic = synthetic %>% relocate(Person_Income)
  
  #Create Feather Directory under ML Algo!!
  setwd(sprintf(root_dir, "/ML Algo/Feather"))
  write_feather(srs, paste0("0", i, "_srs_sample.feather"))
  write_feather(importance, paste0("0", i, "_importance_sample.feather"))
  write_feather(synthetic, paste0("0", i, "_synthetic_sample.feather"))
  }
  
  #due to indexing, if statement is required (instead of 010 we have 10 in this loop)
  if (i >= 10){
    setwd(sprintf(root_dir, "/ML Algo/RDS"))
    srs <- readRDS(paste0(i, "_sample.rds"))$SRS$Base
    importance <- readRDS(paste0(i, "_sample.rds"))$SRS$importance_sampling
    synthetic <- readRDS(paste0(i, "_sample.rds"))$SRS$synthetic_sampling
    
    srs$Sex = as.numeric(as.factor(unique(srs$Sex)))
    #same issue as above
    #importance$Sex = as.numeric(as.factor(unique(importance$Sex)))
    #synthetic$Sex = as.numeric(as.factor(unique(synthetic$Sex)))
    
    srs = srs %>% relocate(Person_Income) 
    importance = importance %>% relocate(Person_Income)
    synthetic = synthetic %>% relocate(Person_Income)
    
    setwd(sprintf(root_dir, "/ML Algo/Feather"))
    write_feather(srs, paste0(i, "_srs_sample.feather"))
    write_feather(importance, paste0(i, "_importance_sample.feather"))
    write_feather(synthetic, paste0(i, "_synthetic_sample.feather"))  
  }
}

