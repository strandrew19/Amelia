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
BUCKET_SIZE <- 20000
formals(compute_income_diff)$bucket_size <- BUCKET_SIZE # Set as global preset

# Get maximum income 
MAX <- max(amelia_full_income)
formals(compute_income_diff)$max_val <- MAX

#### Computing true buckets #####
AMELIA_BUCKET_INCOME <- get_bucket_dist(amelia_full_income, AMELIA = T, max_val = MAX, bucket_size = BUCKET_SIZE)
formals(compute_income_diff)$amelia_income_dist <- AMELIA_BUCKET_INCOME # Set as global preset
formals(resample)$amelia_buckets <- AMELIA_BUCKET_INCOME


#### Simulation #### 

FILENAMES <- str_pad(1:10, width = 2, pad = "0")
FILENAMES <- paste0(FILENAMES, "_sample.rds", sep = "")

SAMPLING_METHODS <- c("SRS", "Stratified", "Stratified Cluster")

for (file in FILENAMES){
  print(sprintf("---------- %s ----------", file))
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  for (method in SAMPLING_METHODS){
    print(sprintf("### %s ####", method))
    
    resample_data <- current_sample[[method]]$Base
    
    income_difference <- compute_income_diff(resample_data$Person_Income)
    
    print(sprintf("Base Sample difference: %.6f", income_difference$squared_deviation))
    
    current_sample[[method]]$importance_sampling <- resample(income_difference, data = resample_data, print_cases = F)
    
    imp_samp_error <- compute_income_diff(current_sample[[method]]$importance_sampling$Person_Income, return_full = F)
    
    print(sprintf("Importance sampling difference: %.8f", imp_samp_error))
  }
}
