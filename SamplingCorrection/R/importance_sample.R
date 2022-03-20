#' Apply importance sampling to a sample to match a given distribution
#'
#' @description Function to perform under- and oversampling per income buckets.
#' Since the density for lower income buckets is much higher than for higher income buckets, we
#' need to put more emphasis on smaller differences in higher income buckets. For this, we
#' scale the 'true' density per bucket to 1 and apply the same scaling to the sample values. We then
#' compute the difference between the two.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr slice_sample
#'
#' @param sample_diff List object from compute_income_diff
#' @param amelia_buckets Density of each income bucket in full AMELIA dataset, used for scaling the density per bucket
#' @param buckets Income buckets
#' @param data Sample data to modify
#' @param print_cases Whether function should print # of replacements per bucket
#'
#' @return dataframe with added and removed data points
#' @export


importance_sample <- function(sample_diff, amelia_buckets, buckets, data, print_cases = T) {
  proportions <- as.numeric(sample_diff$difference_table_prop)


  #' Copy the data frame to append / subtract the additional samples
  #' without modifying the original data frame. I.e. create a copy, not just
  #' an additional pointer to the same object.
  resample_data <- data.frame(data)

  # Scaling
  scale_factor <- 1 / amelia_buckets$dist_prop
  proportions <- proportions * scale_factor


  #' buckets begins with 0, i.e. use ith element as lower and i+1th element as upper border.
  for (i in 1:length(proportions)) {
    # Select data in relevant income bucket
    sample_base <- filter(data, Person_Income >= buckets[i] & Person_Income < buckets[i + 1])

    # Select amount of necessary over/undersampling based on scaled proportional difference
    sample_proportion <- proportions[i]


    # Sample additional data
    add_data <- slice_sample(sample_base, prop = abs(sample_proportion), replace = F)
    number_of_new_cases <- nrow(add_data)


    if (sample_proportion > 0 & number_of_new_cases != 0) {
      if (print_cases) print(sprintf("Bucket %s to %s: Adding %d cases", buckets[i], buckets[i + 1], nrow(add_data)))
      resample_data <- rbind(resample_data, add_data)
    } else if (sample_proportion < 0 & number_of_new_cases != 0) {
      if (print_cases) print(sprintf("Bucket %s to %s: Removing %d cases", buckets[i], buckets[i + 1], nrow(add_data)))
      resample_data <- filter(resample_data, !Personal_ID %in% add_data$Personal_ID)
    }
  }

  return(resample_data)
}