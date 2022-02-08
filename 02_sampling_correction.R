wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

library(arrow)
library(dplyr)
library(stringr)
devtools::load_all("functions/SamplingCorrection")

amelia_full_income <- read_feather(sprintf("%s/data/AMELIA.feather", wd))$Person_Income


##### Get reference values (i.e. 'true' population values) ##### 

# Set bucket size for comparison between values 

# Source: https://unstats.un.org/unsd/demographic/sources/census/quest/NZL2013enIn.pdf
BUCKETS <- c(seq(0, 40000, by = 5000), seq(50000, 70000, by = 10000), 100000, 150000, max(amelia_full_income)+1)

#### Computing true buckets #####
AMELIA_BUCKET_INCOME <- get_bucket_dist(amelia_full_income, buckets = BUCKETS)

#### Simulation #### 

FILENAMES <- str_pad(1:10, width = 2, pad = "0")
FILENAMES <- paste0(FILENAMES, "_sample.rds", sep = "")

SAMPLING_METHODS <- c("SRS", "Stratified", "Stratified Cluster")
seed <- 1

for (file in FILENAMES){
  print(sprintf("---------- %s ----------", file))
  set.seed(seed)
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  
  current_iter <- str_pad(seed, width = 2, pad = "0")
  
  for (method in SAMPLING_METHODS){
    print(sprintf("### %s ####", method))

    resample_data <- current_sample[[method]]
    resample_data$Sex <- as.factor(resample_data$Sex)
    resample_data <- relocate(resample_data, Person_Income) # Put DV as first column for ML

    # Base error
    income_difference <- compute_income_diff(resample_data$Person_Income,
                                             amelia_income_dist = AMELIA_BUCKET_INCOME,
                                             buckets = BUCKETS,
                                             plot_relevance = T)
    print(sprintf("Base Sample difference: %.6f", income_difference$squared_deviation))
    
    ### Saving to feather ####
    filename <- sprintf("%s/data/feather_samples/%s_%s_base_sample.feather", wd, current_iter, method)
    write_feather(resample_data, filename)
    
    # Importance Sampling
    importance_sampling <- importance_sample(income_difference, 
                                             data = resample_data,
                                             amelia_buckets = AMELIA_BUCKET_INCOME,
                                             buckets = BUCKETS,
                                             print_cases = F)
    imp_samp_error <- compute_income_diff(importance_sampling$Person_Income, 
                                          amelia_income_dist = AMELIA_BUCKET_INCOME,
                                          buckets = BUCKETS,
                                          return_full = F)
    print(sprintf("Importance sampling difference: %.6f", imp_samp_error))
    
    ### Saving to feather ####
    filename <- sprintf("%s/data/feather_samples/%s_%s_importance_sample.feather", wd, current_iter, method) 
    write_feather(importance_sampling, filename)
    
    # Synthetic oversampling
    synthetic_sampling <- synth_sample(income_difference, 
                                       data = resample_data,
                                       amelia_buckets = AMELIA_BUCKET_INCOME,
                                       buckets = BUCKETS,
                                       print_cases = F)
    synth_samp_error <- compute_income_diff(synthetic_sampling$Person_Income, 
                                            amelia_income_dist = AMELIA_BUCKET_INCOME,
                                            buckets = BUCKETS,
                                            return_full = F) 
    print(sprintf("Synthetic sampling difference: %.6f", imp_samp_error))
    
    
    ### Saving to feather ####
    filename <- sprintf("%s/data/feather_samples/%s_%s_synthetic_sample.feather", wd, current_iter, method) 
    write_feather(synthetic_sampling, filename)
  }
  
  seed <- seed+1
}
