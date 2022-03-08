#' Compute the difference in the income distribution between a given sample and the full AMELIA dataset.
#'
#' @description This function is a wrapper for all the other function in this file. Note that we use the 'dist_prop'
#' attribute for the difference as this is a measure that has been adjusted by the sample size. If
#' we were to use absolute values, we wouldn't get relevant results as the N for AMELIA is too large.
#'
#' An important step in this function is the computation of the difference between the 'actual' and the sampled
#' income distribution. Here, we did not simply subtract the relative densities, as this over-emphasizes the relevance of
#' deviations in the lower income groups, because the density is highest in these groups to begin with. To counteract this
#' bias, we compute a dampening factor for each bucket, which is simply 1 - density of the specific bucket in either the sample
#' or the true base population.
#'
#' @importFrom scales rescale
#'
#' @param sample_income Vector containing the income values of all the people in the sample
#' @param amelia_income_dist Distribution of the AMELIA income in buckets, i.e. the output of the get_bucket_dist function for the full AMELIA dataset.
#' @param buckets Set vector of income buckets
#' @param plot Whether plot should be generated
#' @param label_distance Distance between labels, only used for plotting
#' @param show_mean  Whether the (scaled) mean should be shown in the plot as a threshold
#' @param show_median Whether the (scaled) median should be shown in the plot as a threshold
#' @param sample_type Sampling method used, only used for the title of the plot
#' @param return_full Returns full list of deviations if TRUE, returns only squared deviation if FALSE
#'
#' @returns
#' If (return_full == TRUE)
#' - List consisting of the difference between the 'true' income in buckets and the sampled income in buckets in three forms:
#'   - dist_val = Absolute # of people in sample per bucket
#'   - dist_prop = Proportion of people in sample per bucket
#'   - difference_table_prop = Difference in table form (used for analysis of sampling methods)
#'   - difference_table_prop_scaled = Difference table with scaled values (range = 0,1)
#'   - squared_deviation = Squared deviation of density per bucket in the sample and true distribution
#' Else
#'   - squared_deviation  = Squared deviation of density per bucket in the sample and true distribution
#'
#' @export


compute_income_diff <- function(sample_income, amelia_income_dist, buckets,
                                plot = F, show_mean = F, show_median = F, sample_type = NULL, plot_relevance = F,
                                return_full = T){

  sample_income_dist <- get_bucket_dist(sample_income, buckets)

  diff_prop <- (amelia_income_dist$dist_prop - sample_income_dist$dist_prop)
  diff_numeric <- as.numeric(diff_prop)

  # Apply rescaling as UBL relevance function ~ [0,1] and convert back to table
  diff_scaled <- as.table(rescale(diff_numeric, to = c(0, 1)))
  names(diff_scaled) <- names(sample_income_dist$dist_prop)

  squared_deviation <- sum(as.numeric(diff_prop)^2)

  all_vals <- list(
    "dist_val" = sample_income_dist$dist_val,
    "dist_prop" = sample_income_dist$dist_prop,
    "difference_table_prop" = diff_prop,
    "difference_table_prop_scaled" = diff_scaled,
    "squared_deviation" = squared_deviation)

  if(plot) plot_income_diff(all_vals, amelia_income_dist, show_mean, show_median, sample_type, plot_relevance)


  ifelse(return_full, return(all_vals), return(squared_deviation))
}
