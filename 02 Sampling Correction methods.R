<<<<<<< HEAD
library(arrow)
library(dplyr)
library(UBL)

setwd("~/Uni/02 Data Science/03 Wise 21-22/Research Case Studies/Code/Amelia")

##### Get reference values (i.e. 'true' population values) ##### 

amelia <- read_feather("data/AMELIA.feather")
amelia_full_income <- amelia$Person_Income

##### Examplary sample #####

set.seed(1)
sample_size <- 100
ex_sample <- data.frame(sample_n(amelia, size = 100))

##### Part 2 ##### 

source("functions/relevance_function.R")

# Set bucket size for comparison between values 
bucket_size <- 20000
formals(get_bucket_dist)$bucket_size <- bucket_size
formals(compute_income_diff)$bucket_size <- bucket_size

# To get reference for the relevance function, we need to create bucktes of the true income distribution
amelia_bucket_income <- get_bucket_dist(amelia_full_income)

# Set AMELIA buckets as global preset for amelia_income_dist in relevance function
formals(compute_income_diff)$amelia_income_dist <- amelia_bucket_income

test <- compute_income_diff(ex_sample$Person_Income, plot = F)


#### Importance sampling ####  

#' Important: These functions assume to receive a data.frame object and they will not work with 
#' other formats, e.g. a tibble (usually returned by dplyr functions).

regression_formula <- Person_Income ~ AGE * Sex + Work_Status

importance_sample <- WERCSRegress(form = Person_Income ~ Household_ID, dat = ex_sample, thr.rel = test$relevance_threshold$mean)
smoteR <- SmoteRegress(form = Person_Income ~ Household_ID, dat = ex_sample, dist = "HEOM", p = 1.3)
=======
# Read data first as RStudio gets stuck on read_feather command otherwise

amelia_full_income <- arrow::read_feather("~/Uni/02 Data Science/03 Wise 21-22/Research Case Studies/Code/Amelia/data/AMELIA.feather")$Person_Income

setwd("~/Uni/02 Data Science/03 Wise 21-22/Research Case Studies/Code/Amelia/")
library(dplyr)
library(UBL)
library(stringr)
source("functions/relevance_function.R")

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

#### Computing sample difference ####
# FILENAMES <- str_pad(1:10, width = 2, pad = "0")
# FILENAMES <- paste0(FILENAMES, "_sample.rds", sep = "")
# 
# SAMPLING_METHODS <- c("SRS", "Stratified", "Stratified Cluster")


ex_sample <- readRDS("data/samples/03_sample.rds")
ex_sample$SRS$Base$Sex <- as.factor(ex_sample$SRS$Base$Sex)

sample_difference <- compute_income_diff(ex_sample$SRS$Base$Person_Income, plot = T, plot_relevance = T)

#### Importance sampling ####  

#' Note:
#' These functions assume to receive a data.frame object and they will not work with 
#' other formats, e.g. a tibble (usually returned by dplyr functions, e.g. when sampling).

REGRESSION_FORMULA <- Person_Income ~ AGE * Sex + Work_Status
resample_data <- ex_sample$SRS$Base

### Relevance function must be supplied as a matrix of form y, phi(y), phi'(y)

imp_samp <- WERCSRegress(
  form = REGRESSION_FORMULA, 
  dat = resample_data,
  rel = sample_difference$relevance_matrix_ubl)

compute_income_diff(imp_samp$Person_Income, plot = T)

smote <- SmoteRegress(form = REGRESSION_FORMULA,
             dat = resample_data,
             dist = "HEOM",
             rel = sample_difference$relevance_matrix_ubl,
             k = 2)

compute_income_diff(smote$Person_Income, plot = T)

#### Simulation #### 

for (file in FILENAMES){
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  for (method in SAMPLING_METHODS){
    income_difference <- compute_income_diff(current_sample[[method]]$Base$Person_Income)
  }
}
>>>>>>> 8535ee84833ad5060e140d119064554b4aad0b4c
