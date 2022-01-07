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
                                padding = 0, damp = "pop",
                                plot = F, label_distance = 10, show_mean = F, show_median = F, sample_type = NULL, plot_relevance = F){
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
  #'   
  #' 
  #' OUTPUT
  #'   - List consisting of the difference between the 'true' income in buckets and the sampled
  #'     income in buckets in three forms:
  #'     - buckets                 = Buckets for given bucket_size
  #'     - dist_val                = Absolute # of people in sample per bucket
  #'     - dist_prop               = Proportion of people in sample per bucket
  #'     - difference_table_abs    = Difference in table form (absolute values)
  #'     - difference_table_prop   = Difference in table form (used for analysis of sampling methods)
  #'     - scaled_difference_table = Difference table with scaled values 
  #'     - relevance_table         = Difference in table form with dampening to counteract group size
  #'     - scaled_relevance_table  = Relevance table with scaled values 
  #'     - relevance_matrix_ubl    = Relevance in correct form for functions in UBL package
  #'     - relevance_threshold     = Suggested threshold values for relevance function. Contains mean and
  #'                                 median of the scaled relevance function as a list.
  
  min <- 0
  max <- max_val + bucket_size
  
  buckets <- seq(min, max, bucket_size)
  
  sample_income_dist <- get_bucket_dist(sample_income, buckets = buckets)
  
  if (tolower(damp) == "pop") dampening <- 1 - amelia_income_dist$dist_prop
  else if (tolower(damp) == "sample") dampening <- 1 - sample_income_dist$dist_prop
  else{
    message("Input for 'damp' must be either 'sample' or 'pop'. Defaulting to 'pop'.")
    dampening <- 1 - amelia_income_dist$dist_prop
  }
  
  diff_abs <- (amelia_income_dist$dist_val - sample_income_dist$dist_val)
  diff_prop <- (amelia_income_dist$dist_prop - sample_income_dist$dist_prop)
  diff_numeric <- as.numeric(diff_prop)
  relevance <- diff_prop * dampening
  
  # Apply rescaling as UBL relevance function ~ [0,1] and convert back to table
  min <- 0 + padding
  max <- 1 - padding
  
  diff_scaled <- as.table(rescale(diff_numeric, to = c(min, max)))
  relevance_scaled <- as.table(rescale(diff_numeric, to = c(min, max)))
  
  names(diff_scaled) <- names(sample_income_dist$dist_prop)
  names(relevance_scaled) <- names(sample_income_dist$dist_prop)
  
  # Create matrix for UBL functions 
  # Shape: x, phi(x), phi'(x)

  # To get phi'(x): Spline interpolation and get first derivative
  fun <- splinefun(x = buckets[1:length(buckets)-1], y = as.numeric(relevance_scaled))
  
  
  relevance_deriv_per_bucket <- fun(x = buckets[1:length(buckets)-1], deriv = 1)
  
  relevance_matrix_ubl <- matrix(
    cbind(
      buckets[1:length(buckets)-1] + 1/2 * bucket_size, # Mean of each bucket as sampling point 
      as.numeric(relevance_scaled) * (1- as.numeric(relevance_scaled)), 
      relevance_deriv_per_bucket), 
    ncol = 3)
  
  
  
  # Suggested Threshold for relevance function: Median of relevance function
  threshold_mean <- mean(as.numeric(relevance_scaled))
  threshold_med <- median(as.numeric(relevance_scaled))
  
  all_vals <- list(
    "buckets" = buckets,
    "dist_val" = sample_income_dist$dist_val,
    "dist_prop" = sample_income_dist$dist_prop,
    "difference_table_abs" = diff_abs,
    "difference_table_prop" = diff_prop, 
    "scaled_difference_table" = diff_scaled,
    "relevance_table" = relevance, 
    "scaled_relevance_table" = relevance_scaled,
    "relevance_matrix_ubl" = relevance_matrix_ubl,
    "relevance_threshold" = list("mean" = threshold_mean,
                                 "median" = threshold_med))
  
  if(plot) plot_income_diff(all_vals, amelia_income_dist, bucket_size, label_distance, padding, show_mean, show_median, sample_type, plot_relevance)
  
  return(all_vals)
}
