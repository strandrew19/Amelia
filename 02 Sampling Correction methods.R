# Read data first as RStudio gets stuck on read_feather command otherwise
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
amelia_full_income <- arrow::read_feather(sprintf("%s/data/AMELIA.feather", wd))$Person_Income

setwd(wd)
library(dplyr)
library(UBL)
library(stringr)
source("functions/relevance_function.R")
source("functions/relevance_function_plot.R")
source("functions/sampling_correction.R")

##### Get reference values (i.e. 'true' population values) ##### 

# Set bucket size for comparison between values 

# Source: https://unstats.un.org/unsd/demographic/sources/census/quest/NZL2013enIn.pdf
BUCKETS <- c(seq(0, 40000, by = 5000), seq(50000, 70000, by = 10000), 100000, 150000, max(amelia_full_income)+1)
formals(compute_income_diff)$buckets <- BUCKETS # Set as global preset
formals(importance_sample)$buckets <- BUCKETS
formals(synth_sample)$buckets <- BUCKETS

#### Computing true buckets #####
AMELIA_BUCKET_INCOME <- get_bucket_dist(amelia_full_income, buckets = BUCKETS)

formals(compute_income_diff)$amelia_income_dist <- AMELIA_BUCKET_INCOME # Set as global preset
formals(importance_sample)$amelia_buckets <- AMELIA_BUCKET_INCOME
formals(synth_sample)$amelia_buckets <- AMELIA_BUCKET_INCOME

#### Simulation #### 

FILENAMES <- str_pad(1:10, width = 2, pad = "0")
FILENAMES <- paste0(FILENAMES, "_sample.rds", sep = "")

SAMPLING_METHODS <- c("SRS", "Stratified", "Stratified Cluster")
seed <- 1

for (file in FILENAMES){
  print(sprintf("---------- %s ----------", file))
  set.seed(seed)
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  for (method in SAMPLING_METHODS){
    print(sprintf("### %s ####", method))

    resample_data <- current_sample[[method]]$Base
    resample_data$Sex <- as.factor(resample_data$Sex)

    # Base error
    income_difference <- compute_income_diff(resample_data$Person_Income, plot = T, plot_relevance = T)
    print(sprintf("Base Sample difference: %.6f", income_difference$squared_deviation))
    
    # Importance Sampling
    current_sample[[method]]$importance_sampling <- importance_sample(income_difference, data = resample_data, print_cases = F)
    imp_samp_error <- compute_income_diff(current_sample[[method]]$importance_sampling$Person_Income, return_full = F)
    print(sprintf("Importance sampling difference: %.6f", imp_samp_error))
    
    # Synthetic oversampling
    current_sample[[method]]$synthetic_sampling <- synth_sample(income_difference, data = resample_data, k = 3, print_cases = F)
    synth_samp_error <- compute_income_diff(current_sample[[method]]$synthetic_sampling$Person_Income, return_full = F)
    print(sprintf("Synthetic sampling difference: %.6f", imp_samp_error))
  }
  
  # Save
  filename <- sprintf("data/samples/%s", file)
  saveRDS(current_sample, filename)
  
  seed <- seed + 1
}