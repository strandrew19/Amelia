# Read data first as RStudio gets stuck on read_feather command otherwise
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
amelia_full_income <- arrow::read_feather(sprintf("%s/data/AMELIA.feather", wd))$Person_Income

setwd(wd)
library(dplyr)
library(UBL)
library(stringr)
source("functions/relevance_function.R")
source("functions/sampling_correction.R")
source("functions/relevance_function_plot.R")

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



#### Simulation #### 

for (file in FILENAMES){
  current_sample <- readRDS(sprintf("data/samples/%s", file))
  for (method in SAMPLING_METHODS){
    income_difference <- compute_income_diff(current_sample[[method]]$Base$Person_Income)
  }
}
