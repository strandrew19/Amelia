library(ggplot2)

rescale <- scales::rescale

get_bucket_dist <- function(x, buckets, AMELIA = F, max_val = NULL, bucket_size = NULL){
  #' This function computes the buckets for a given income sample, which is needed to compare it 
  #' to the known information about the income distribution of the full dataset.
  #' 
  #' INPUTs
  #'   - x           = income of sample
  #'   - buckets     = vector of buckets based on AMELIA dataset
  #'   - AMELIA      = T/F variable, indicates whether buckets on original AMELIA data are computed
  #'   - max_val     = maximum income value, only relevant if AMELIA == T
  #'   - bucket_size = size of income buckets, only relevant if AMELIA == T
  #'  
  #'  OUTPUT
  #'    - a list with
  #'      - amount people per income class (table) 
  #'      - the proportion of people per income class (prop.table)
  
  
  if (AMELIA){
    min <- 0
    max <- max_val + bucket_size
    
    buckets <- seq(min, max, bucket_size)
  }
  
  dist_val <- table(cut(x, breaks = buckets, include.lowest = T))
  dist_prop <- prop.table(dist_val)
  
  return(list("dist_val" = dist_val, "dist_prop" = dist_prop))
}

compute_income_diff <- function(sample_income, amelia_income_dist, bucket_size, max_val,
                                plot = F, label_distance = 10, show_mean = F, show_median = F, sample_type = NULL, plot_relevance = F,
                                return_full = T){
  #' This function is a wrapper for all the other function in this file. Note that we use the 'dist_prop'
  #' attribute for the difference as this is a measure that has been adjusted by the sample size. If 
  #' we were to use absolute values, we wouldn't get relevant results as the N for AMELIA is too large.
  #' 
  #' An important step in this function is the computation of the difference between the 'actual' and the sampled
  #' income distribution. Here, we did not simply subtract the relative densities, as this over-emphasizes the relevance of
  #' deviations in the lower income groups, because the density is highest in these groups to begin with. To counteract this
  #' bias, we compute a dampening factor for each bucket, which is simply 1 - density of the specific bucket in either the sample
  #' or the true base population.
  #' 
  #' INPUT
  #'   - sample_income      = Vector containing the income values of all the people in the sample
  #'   - amelia_income_dist = Distribution of the AMELIA income in buckets, i.e. the output 
  #'                          of the get_bucket_dist function for the full AMELIA dataset.
  #'   - bucket_size        = Size of income buckets in information about AMELIA full population
  #'   - max                = maximum income in AMELIA dataset
  #'   - padding            = Potential padding of the relevance, ranges between 0 and 0.5.
  #'                          Determines how much smaller than 0/1 the min/max values are. 
  #'   - damp               = Specifies the density that should be used for dampening the relevance function. 
  #'                          Can be either "sample" or "pop", if anything else is entered it defaults to pop.
  #'   - plot               = Whether plot should be generated
  #'   - label_distance     = Distance between labels, only used for plotting
  #'   - show_mean          = Whether the (scaled) mean should be shown in the plot as a threshold
  #'   - show_median        = Whether the (scaled) median should be shown in the plot as a threshold
  #'   - sample_type        = Sampling method used, only used for the title of the plot
  #'   - return_full        = Returns full list of deviations if TRUE, returns only squared deviation if FALSE
  #'   
  #' 
  #' OUTPUT
  #'   - If (return_full == TRUE)
  #'     - List consisting of the difference between the 'true' income in buckets and the sampled
  #'       income in buckets in three forms:
  #'       - buckets                      = Buckets for given bucket_size
  #'       - dist_val                     = Absolute # of people in sample per bucket
  #'       - dist_prop                    = Proportion of people in sample per bucket
  #'       - difference_table_abs         = Difference in table form (absolute values)
  #'       - difference_table_prop        = Difference in table form (used for analysis of sampling methods)
  #'       - difference_table_prop_scaled = Difference table with scaled values (range = 0,1)
  #'       - squared_deviation            = Squared deviation of density per bucket in the sample and true 
  #'                                        distribution
  #'   - Else
  #'       - squared_deviation            = Squared deviation of density per bucket in the sample and true 
  #'                                        distribution

  buckets <- seq(0, max_val + bucket_size, bucket_size)
  
  sample_income_dist <- get_bucket_dist(sample_income, buckets = buckets)
  
  diff_abs <- (amelia_income_dist$dist_val - sample_income_dist$dist_val)
  diff_prop <- (amelia_income_dist$dist_prop - sample_income_dist$dist_prop)
  diff_numeric <- as.numeric(diff_prop)
  
  # Apply rescaling as UBL relevance function ~ [0,1] and convert back to table
  diff_scaled <- as.table(rescale(diff_numeric, to = c(0, 1)))
  names(diff_scaled) <- names(sample_income_dist$dist_prop)
  
  squared_deviation <- sum(as.numeric(diff_prop)^2)
  
  all_vals <- list(
    "buckets" = buckets,
    "dist_val" = sample_income_dist$dist_val,
    "dist_prop" = sample_income_dist$dist_prop,
    "difference_table_val" = diff_abs,
    "difference_table_prop" = diff_prop, 
    "difference_table_prop_scaled" = diff_scaled,
    "squared_deviation" = squared_deviation)
  
  if(plot) plot_income_diff(all_vals, amelia_income_dist, bucket_size, label_distance, show_mean, show_median, sample_type, plot_relevance)
  
  
  if(return_full) return(all_vals)
  else return(squared_deviation)
}
