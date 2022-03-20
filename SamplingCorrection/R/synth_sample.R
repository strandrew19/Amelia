#' Apply synthetic sampling using kNN to a sample to match a given distribution
#'
#' @description Synthetically generate new data points using the kNN function from the FNN package.
#' Applies the same scaling of relevance to higher values, but does not generate any new
#' data points if there are less data points in the given cluster than new ones would
#' be required.
#'
#' @import dplyr
#' @importFrom FNN get.knnx
#'
#' @param List object from compute_income_diff
#' @param Sample data to modify
#' @param Density of each income bucket in full AMELIA dataset, used for scaling the density per bucket
#' @param buckets Income buckets
#' @param k # of neighbours to compare to, defaults to 4
#' @param print_cases Whether function should print # of replacements per bucket
#'
#' @return Dataframe with added data points
#'
#' @export


synth_sample <- function(sample_diff, data, amelia_buckets, buckets, k = 4, print_cases = T){

  proportions <- as.numeric(sample_diff$difference_table_prop)

  # Scaling
  scale_factor <- 1/amelia_buckets$dist_prop
  proportions <- proportions * scale_factor

  # Since we do kNN, we can only add cases, i.e. shift s.t. lowest value == 0
  no_of_cases <- proportions * sample_diff$dist_val
  min_no_of_cases <- min(no_of_cases)
  no_of_cases <- round(no_of_cases + ifelse(min_no_of_cases < 0, abs(min_no_of_cases), min_no_of_cases))

  synthetic_cases <- data.frame()
  for (i in 1:length(proportions)){
    if(no_of_cases[i] != 0 & no_of_cases[i] < sample_diff$dist_val[i]){
      # Get all cases in current income bucket
      knn_base <- data %>%
        filter(Person_Income >= buckets[i] & Person_Income < buckets[i+1]) %>%
        # Convert to numeric for kNN implementation to work.
        mutate_all(function(x) as.numeric(x)) # Potential problem: Do we need Sex as dummy-coding?

      #' Do random split into to datasets. One contains as many cases as we need to generate new
      #' data points. Use these data points as basis, find k-1 nearest neighbours in other cases.
      new_cases_basis <- knn_base %>% sample_n(no_of_cases[i], replace = F)
      other_cases <- knn_base %>% filter(!Personal_ID %in% new_cases_basis$Personal_ID)

      # Get NNs, account for small sample sizes when choosing k
      nn <- ifelse(k < nrow(other_cases), k, nrow(other_cases))
      nn_data <- get.knnx(data = other_cases, query = new_cases_basis, k = nn)

      current_synthethic_cases <- data.frame()
      for (j in 1:nrow(new_cases_basis)){
        # Get current case from new_cases basis and it's k nearest neighbours based on their index
        current_cases <- rbind(new_cases_basis[j,], other_cases[nn_data$nn.index[j,],])

        # Compute new value as the mean of the k nearest neighbours
        new_case <- current_cases %>% colMeans() %>% as.data.frame.list()

        # Append to dataframe
        current_synthethic_cases <- rbind(current_synthethic_cases, new_case)
      }

      if(print_cases) print(sprintf("Bucket %s to %s: Generating %d new cases", buckets[i], buckets[i+1], nrow(current_synthethic_cases)))
      synthetic_cases <- rbind(synthetic_cases, current_synthethic_cases)
    }
  }

  # Convert integer values back to integer
  synthetic_cases[sapply(data, class) == "integer",] <- sapply(synthetic_cases[sapply(data, class) == "integer",], as.integer)

  # Convert sex back to factor by rounding
  synthetic_cases$Sex <- factor(round(synthetic_cases$Sex), levels = c(1,2), labels = c("Female", "Male"))

  resample_data <- rbind(data, synthetic_cases)

  return(resample_data)
}
