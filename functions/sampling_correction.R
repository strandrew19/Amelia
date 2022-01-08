folder <- dirname(rstudioapi::getSourceEditorContext()$path)



library(dplyr)

##### Importance Sampling ######

resample <- function(sample_diff, amelia_buckets, data, smoteR = F){
  
  #' Function to perform under- and oversampling per income buckets. 
  #' Since the density for lower income buckets is much higher than for higher income buckets, we
  #' need to put more emphasis on smaller differences in higher income buckets. For this, we 
  #' scale the 'true' density per bucket to 1 and apply the same scaling to the sample values. We then 
  #' compute the difference between the two. 
  #' 
  #' INPUT: 
  #'   - sample_diff    = List object from compute_income_diff
  #'   - data           = Sample data to modify
  #'   - amelia_buckets = Density of each income bucket in full AMELIA dataset, used for scaling
  #'                      the density per bucket 
  #'   - smoteR         = Whether or not kNN should be used to generate synthetic data points
  #' 
  #' OUTPUT
  #'   - resample_data = data with added/removed data points
  
  buckets <- sample_diff$buckets                      
  proportions <- as.numeric(sample_diff$difference_table_prop)
  relevance <- as.numeric(sample_diff$relevance_table)
  
  #' Copy the data frame to append / subtract the additional samples
  #' without modifying the original data frame. I.e. create a copy, not just
  #' an additional pointer to the same object.
  resample_data <- data.frame(data)
  
  # Scaling
  scale_factor <- 1/amelia_buckets$dist_prop
  proportions <- proportions * scale_factor

  #' buckets begins with 0 and goes farther than maximum Income in full AMELIA dataset.
  #' Skip first element and use each as upper bound for the current income bucket
  #' We can also use i-1 on proportions to get the proportional sample that we need to 
  #' add/subtract in the current class. We have length(buckets) == length(proportions) +1
  for (i in 2:length(buckets)){
    # Select data in relevant income bucket
    sample_base <- data %>% filter(Person_Income >= buckets[i-1] & Person_Income < buckets[i])
  
    # Select amount of necessary over/undersampling based on scaled proportional difference
    sample_proportion <- proportions[i-1]
    
    # Sample additional data    
    add_data <- slice_sample(sample_base, prop = abs(sample_proportion), replace = F)
    number_of_new_cases <- nrow(add_data)
    
    
    if (sample_proportion > 0 & number_of_new_cases != 0){
      print(sprintf("Bucket %s to %s: Adding %d cases", buckets[i-1], buckets[i], nrow(add_data)))
      resample_data <- rbind(resample_data, add_data)
    } else if (sample_proportion < 0 & number_of_new_cases != 0) {
      print(sprintf("Bucket %s to %s: Removing %d cases", buckets[i-1], buckets[i], nrow(add_data)))
      resample_data <- filter(resample_data, !Personal_ID %in% add_data$Personal_ID)
    }
  }
  
  return(resample_data) 
}

resample_data <- ex_sample$`Stratified Cluster`$Base
sample_difference <- compute_income_diff(ex_sample$`Stratified Cluster`$Base$Person_Income, plot = T, plot_relevance = T)

test <- resample(sample_difference, AMELIA_BUCKET_INCOME, data = resample_data)
irrel <- compute_income_diff(test$Person_Income, plot = T, plot_relevance = T)

##### SmoteR ##### 

#' GIVEN
#'   - Table of data points
#'   - Proportion of new cases to generate
#'    

# GenSynthCases
knn_data <- resample_data %>% filter(Person_Income > 20000 & Person_Income <= 40000)





