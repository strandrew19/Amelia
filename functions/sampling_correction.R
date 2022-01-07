folder <- dirname(rstudioapi::getSourceEditorContext()$path)

sample_difference <- compute_income_diff(ex_sample$Stratified$Base$Person_Income)


library(dplyr)
library(class)

##### Importance Sampling ######

over_under_sample <- function(sample_diff, data, use_relevance = F, thresh = NULL){
  
  #' Function to perform under- and oversampling per income buckets. Two methods of determining
  #' additional samples and whether to add or remove cases: 
  #'   1. Based on sample_propotion: 
  #'       - Oversample if difference in density between sample and AMELIA is > 0, undersample if < 0
  #'       - Compute amount of necessary sampling as the difference in proportions
  #'       - Scale by some weight that grows very fast, as differences in proportions get much smaller
  #'         for very high income rates due to very few cases overall
  #'   2. Based on relevance function: Add if relevance of bucket > threshold, remove if < threshold
  #'       - Oversample if the relevance of the current bucket is above a specified threshold, 
  #'         undersample if < 0
  #'       - Compute amount of necessary sampling as the difference between the relevance function 
  #'         and the threshold
  #'       - Scale by some weight that grows slower, as relevance function already compensates for
  #'         change in base rate
  #' 
  #' INPUT: 
  #'   - sample_diff   = List object from compute_income_diff
  #'   - data          = Sample data to modify
  #'   - use_relevance = Whether relevance function should be used as measure for 
  #'                     over-/undersampling
  #'   - thresh        = Threshold for over-/undersampling if use_relevance == T
  #' 
  #' OUTPUT
  #'   - resample_data = data with added/removed data points
  
  buckets <- sample_diff$buckets                      
  proportions <- as.numeric(sample_diff$difference_table_prop)
  relevance <- as.numeric(sample_diff$scaled_relevance_table)
  
  #' Copy the data frame to append / subtract the additional samples
  #' without modifying the original data frame. I.e. create a copy, not just
  #' an additional pointer to the same object.
  resample_data <- data.frame(data)
  
  # Put more emphasis on higher incomes (i.e. weight differences in proportions more strongly)
  # If we don't do this, anything > 100.000 will not be changed by this code as the proportions are too small.
  # Maybe we can find a method to do this that is theoretically sound? @Andrew, @Kate
  if (!use_relevance) weight <- (1:length(buckets))^6
  else weight <- (1:length(buckets))^2
  
  
  #' buckets begins with 0 and goes farther than maximum Income in full AMELIA dataset.
  #' Skip first element and use each as upper bound for the current income bucket
  #' We can also use i-1 on proportions to get the proportional sample that we need to 
  #' add/subtract in the current class. This works as length(buckets) = length(proportions) +1
  
  for (i in 2:length(buckets)){
    # Select data in relevant income bucket
    sample_base <- data %>% filter(Person_Income >= buckets[i-1] & Person_Income < buckets[i])
  
    # Select amount of necessary over/undersampling based on proportion or relevance function
    if (!use_relevance) sample_proportion <- proportions[i-1]
    else sample_proportion <- (relevance[i-1] - thresh) * (1 - proportions[i-1])
    
    # Sample additional data    
    add_data <- slice_sample(sample_base, prop = abs(sample_proportion)* weight[i])
    number_of_new_cases <- nrow(add_data)
    
    # Option 1: Sample proportion
    if (!use_relevance){
      compare_val <- sample_proportion
      critical_val <- 0
    } 
    # Option 2: Relevance function
    else{
      compare_val <- relevance[i-1]
      critical_val <- thresh
    }
    
    if (compare_val > critical_val & number_of_new_cases != 0){
      print(sprintf("Bucket %s to %s: Adding %d cases", buckets[i-1], buckets[i], nrow(add_data)))
      resample_data <- rbind(resample_data, add_data)
    } else if (compare_val < critical_val & number_of_new_cases != 0) {
      print(sprintf("Bucket %s to %s: Removing %d cases", buckets[i-1], buckets[i], nrow(add_data)))
      resample_data <- filter(resample_data, !Personal_ID %in% add_data$Personal_ID)
    }
    
    
  }
  
  return(resample_data) 
}

test <- over_under_sample(sample_difference, data = resample_data, use_relevance = T, thresh = sample_difference$relevance_threshold$mean)
nrow(test)

##### SmoteR ##### 

filter(resample_data, !Personal_ID %in% resample_data$Personal_ID[1:99990])


