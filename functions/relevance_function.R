library(ggplot2)
rescale <- scales::rescale

get_bucket_dist <- function(x, bucket_size, max_val = 1034300){
  #' This function computes the buckets for a given income sample, which is needed to compare it 
  #' to the known information about the income distribution of the full dataset.
  #' 
  #' INPUTs
  #'   - x           = income of sample
  #'   - bucket_size = size of outgoing buckets, should be set once and always kept constant
  #'   - max_val     = maximum income in AMELIA dataset (to ensure that we always have same amount 
  #'                   of categories)
  #'  
  #'  OUTPUT
  #'    - a list with
  #'      - amount people per income class (table) 
  #'      - the proportion of people per income class (prop.table)
  
  
  min <- 0
  max <- max_val + bucket_size
  
  buckets <- seq(min, max, bucket_size)
  
  dist_val <- table(cut(x, breaks = buckets, include.lowest = T))
  dist_prop <- prop.table(dist_val)
  
  return(list("dist_val" = dist_val, "dist_prop" = dist_prop))
}

plot_income_diff <- function(sample_data, amelia, bucket_size, label_distance){
  #' This function is used to show the difference in income between the sample and the 
  #' actual distribution in the AMELIA dataset.
  #' 
  #' INPUT
  #'   - sample_data: Output value of compute_income_diff()
  #'   - amelia: Income distribution of original AMELIA dataset with bucket size 
  #'   - bucket_size: Size of buckets, used for binwith
  #'   - label_distance: Each i-th label will be displayed in the plot
  #' 
  #' OUTPUT
  #'   - Histogram of density of distributions + plot of difference
  #'   
  
  plot_data <- data.frame(
    "sample_dist" = as.numeric(sample_data$dist_prop), 
    "amelia_dist" = as.numeric(amelia$dist_prop),
    "diff" = as.numeric(sample_data$scaled_difference_table))
  
  # Plot colors
  fills <- c("Sample population" = "#EE6123", "True population" = "#36BFAA")
  relevance_color <- "#3634CA"
  color <- c("Relevance Function" = relevance_color)
  
  plot_data$names <- factor(names(sample_data$dist_prop), levels = names(sample_data$dist_prop))
  
  # Plot labels 
  labels <- rep("", times = nrow(plot_data))
  
  for (i in 1:length(labels)){
    if ((i-1) %% label_distance == 0) labels[i] <- attributes(plot_data$names)$levels[i]
  }
  
  ### Base plot (distributions) ###
  
  plot <- ggplot(data = plot_data, aes(x = names, width = 1)) + 
    geom_col(aes(y = sample_dist, fill = "Sample population")) +
    geom_col(aes(y = amelia_dist, fill = "True population"), alpha = 0.4) +
    scale_fill_manual(values = fills)
  
  
  ### Relevance function ### 
  
  # Rescale difference of relevance function to fit frame
  max_val <- max(max(plot_data$sample_dist), max(plot_data$amelia_dist))
  
  plot_data$diff <- rescale(plot_data$diff, to = c(0, max_val))
  
  # Adding the plot for the relevance function. This is done in single segments, which is pretty slow.
  
  for (i in 1:length(labels)){
    plot <- plot + 
      geom_segment(x = i-0.5, xend = i+0.5, y = plot_data$diff[i], yend = plot_data$diff[i], lineend = "round")
  }
  
  for (i in 1:length(labels)){
    if (i < length(labels)){
      plot <- plot + 
        geom_segment(aes(color = "Relevance Function"), x = i+0.5, xend = i+0.5, y = plot_data$diff[i], yend = plot_data$diff[i+1], lineend = "round")
    } else {
      plot <- plot + 
        geom_segment(aes(color = "Relevance Function"), x = i+0.5, xend = i+0.5, y = plot_data$diff[i], yend = plot_data$diff[i], lineend = "round")
    }
    
  }
  
  plot <- plot + scale_color_manual(values = color, labels = c("Relevance function"))
  
  ### Theme ###
  
  plot <- plot +
    scale_x_discrete(labels = labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggtitle(label = "Difference between sample and true income distribution",
            subtitle = "Relevance function has been scaled to range between 0 and maximum density value") + 
    guides(fill = guide_legend(title=NULL), color = guide_legend(title=NULL))
  
  print(plot)
}


compute_income_diff <- function(sample_income, amelia_income_dist, bucket_size, padding = 0, plot = T, label_distance = 10){
  #' This function is a wrapper for all the other function in this file. Note that we use the 'dist_prop'
  #' attribute for the difference as this is a measure that has been adjusted by the sample size. If 
  #' we were to use absolute values, we wouldn't get relevant results as the N for AMELIA is too large.
  #' 
  #' INPUT
  #'   - sample_income      = Vector containing the income values of all the people in the sample
  #'   - amelia_income_dist = Distribution of the AMELIA income in buckets, i.e. the output 
  #'                          of the get_bucket_dist function for the full AMELIA dataset
  #'   - padding            = Potential padding of the relevance, ranges between 0 and 0.5.
  #'                          Determines how much smaller than 0/1 the min/max values are. 
  #'   - plot               = Whether plot should be generated
  #'   - label_distance     = Distance between labels, only used for plotting
  #' 
  #' OUTPUT
  #'   - List consisting of the difference between the 'true' income in buckets and the sampled
  #'     income in buckets in three forms:
  #'     - dist_val                = Absolute # of people in sample per bucket
  #'     - dist_prop               = Proportion of people in sample per bucket
  #'     - difference_table        = Difference in table form (used for analysis of methods)
  #'     - scaled_difference_table = Difference table with scaled values (used for UBL functions)
  
  sample_income_dist <- get_bucket_dist(sample_income, bucket_size = bucket_size)
  
  diff <- amelia_income_dist$dist_prop - sample_income_dist$dist_prop
  
  # Apply rescaling as UBL relevance function ~ [0,1]
  min <- 0 + padding
  max <- 1 - padding
  
  diff_scaled <- as.table(rescale(as.numeric(diff), to = c(min, max)))
  names(diff_scaled) <- names(sample_income_dist$dist_prop)
  
  
  all_vals <- list(
    "dist_val" = sample_income_dist$dist_val,
    "dist_prop" = sample_income_dist$dist_prop,
    "difference_table" = diff, 
    "scaled_difference_table" = diff_scaled)
  
  if(plot) plot_income_diff(all_vals, amelia_income_dist, bucket_size, label_distance)
  
  return(all_vals)
}
