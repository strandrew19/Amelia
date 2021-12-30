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
