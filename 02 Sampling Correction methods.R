# Read data first as RStudio gets stuck on read_feather command otherwise

library(arrow)
setwd("~/Uni/02 Data Science/03 Wise 21-22/Research Case Studies/Code/Amelia")
amelia_full_income <- read_feather("data/AMELIA.feather")$Person_Income

library(dplyr)
library(UBL)
library(stringr)
source("functions/relevance_function.R")

##### Get reference values (i.e. 'true' population values) ##### 


# Set bucket size for comparison between values 
BUCKET_SIZE <- 5000
formals(get_bucket_dist)$bucket_size <- BUCKET_SIZE # Set as global preset
formals(compute_income_diff)$bucket_size <- BUCKET_SIZE # Set as global preset


#### Computing true buckets #####
AMELIA_BUCKET_INCOME <- get_bucket_dist(amelia_full_income)
formals(compute_income_diff)$amelia_income_dist <- AMELIA_BUCKET_INCOME # Set as global preset

#### Computing sample difference ####

FILENAMES <- str_pad(1:10, width = 2, pad = "0")
FILENAMES <- paste0(FILENAMES, "_sample.rds", sep = "")

SAMPLING_METHODS <- c("SRS", "Stratified", "Stratified Cluster")




ex_sample <- readRDS("data/samples/03_sample.rds")
sample_difference <- compute_income_diff(ex_sample$SRS$Base$Person_Income, plot = T)

#### Importance sampling ####  

#' Important: These functions assume to receive a data.frame object and they will not work with 
#' other formats, e.g. a tibble (usually returned by dplyr functions).

REGRESSION_FORMULA <- Person_Income ~ AGE * Sex + Work_Status

resample_data <- ex_sample$SRS$Base
relevance_function <- sample_difference$scaled_relevance_table
relevance_threshold <- sample_difference$relevance_threshold$mean

### Relevance function must be supplied as a matrix of form y, phi(y), phi'(y)

buckets <- seq(0, 1034300, BUCKET_SIZE)
relevance_per_bucket <- as.numeric(relevance_function)
relevance_deriv_per_bucket <- rep(0,length(buckets))

relevance_matrix <- matrix(cbind(buckets, relevance_per_bucket, relevance_deriv_per_bucket), ncol = 3)

WERCSRegress(
  form = REGRESSION_FORMULA, 
  dat = resample_data,
  rel = relevance_matrix)

# SmoteRegress(form = REGRESSION_FORMULA, 
#              dat = ex_sample$SRS$Base, 
#              dist = "HEOM", 
#              rel = relevance_matrix)

#### Simulation #### 

for (file in FILENAMES){
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  for (method in SAMPLING_METHODS){
    income_difference <- compute_income_diff(current_sample[[method]]$Base$Person_Income)
  }
}