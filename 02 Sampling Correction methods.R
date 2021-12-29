library(arrow)
library(UBL)
source("functions/relevance_function.R")

##### Get reference values (i.e. 'true' population values) ##### 

amelia_full_income <- read_feather("data/AMELIA.feather")$Person_Income

# Set bucket size for comparison between values 
bucket_size <- 5000
formals(get_bucket_dist)$bucket_size <- bucket_size
formals(compute_income_diff)$bucket_size <- bucket_size

# To get reference for the relevance function, we need to create bucktes of the true income distribution
amelia_bucket_dist <- get_bucket_dist(amelia_full_income)

# Set AMELIA buckets as global preset for amelia_income_dist in relevance function
formals(compute_income_diff)$amelia_income_dist <- amelia_bucket_dist


##### Examplary sample #####
set.seed(10)

sample_size <- 100
ex_sample <- sample(amelia_full_income, size = sample_size, replace = F)

{
  source("functions/relevance_function.R")
  formals(get_bucket_dist)$bucket_size <- bucket_size
  formals(compute_income_diff)$bucket_size <- bucket_size
  formals(compute_income_diff)$amelia_income_dist <- amelia_bucket_dist
  bla <- compute_income_diff(ex_sample, amelia_bucket_dist, bucket_size, label_distance = 10)
}
