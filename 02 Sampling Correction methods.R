library(arrow)
library(UBL)
source("functions/relevance_function.R")

##### Get reference values (i.e. 'true' population values) ##### 

amelia <- read_feather("data/AMELIA.feather")
amelia_full_income <- amelia$Person_Income

# Set bucket size for comparison between values 
bucket_size <- 20000
formals(get_bucket_dist)$bucket_size <- bucket_size
formals(compute_income_diff)$bucket_size <- bucket_size

# To get reference for the relevance function, we need to create bucktes of the true income distribution
amelia_bucket_income <- get_bucket_dist(amelia_full_income)

# Set AMELIA buckets as global preset for amelia_income_dist in relevance function
formals(compute_income_diff)$amelia_income_dist <- amelia_bucket_income

##### Examplary sample #####
set.seed(10)

sample_size <- 100
ex_sample <- dplyr::sample_n(amelia, size = sample_size, replace = F)

test <- compute_income_diff(ex_sample$Person_Income, plot = F)

#### Importance sampling #### 


