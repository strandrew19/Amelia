plot_income_diff <- function(sample_data, amelia, show_mean, show_median, sample_type, plot_relevance){
  #' This function is used to show the difference in income between the sample and the 
  #' actual distribution in the AMELIA dataset.
  #' 
  #' INPUT
  #'   - sample_data = Output value of compute_income_diff()
  #'   - amelia      = Income distribution of original AMELIA dataset with bucket size 
  #'   - padding     = By how much the relevance function has been padded (i.e. by how much the min/max differ from [0,1])
  #'   - show_mean   = Whether mean should be plotted as a horizontal line
  #'   - show_median = Whether median should be plotted as a horizontal line
  #'   - sample_type = Implemented sampling method, gets added to the title if not null
  #' 
  #' OUTPUT
  #'   - Histogram of density of distributions + plot of difference
  
  plot_data <- data.frame(
    "sample_dist" = as.numeric(sample_data$dist_prop), 
    "amelia_dist" = as.numeric(amelia$dist_prop),
    "diff" = as.numeric(sample_data$difference_table_prop_scaled))
  
  # Plot colors
  fills <- c("Sample population" = "#EE6123", "True population" = "#36BFAA")
  
  
  plot_data$names <- factor(names(sample_data$dist_prop), levels = names(sample_data$dist_prop))
  
  ### Base plot (distributions) ###
  
  plot <- ggplot(data = plot_data, aes(x = names, width = 1)) + 
    geom_col(aes(y = sample_dist, fill = "Sample population")) +
    geom_col(aes(y = amelia_dist, fill = "True population"), alpha = 0.4) +
    scale_fill_manual(values = fills)
  
  
  ### Relevance function ### 
  
  # Rescale difference of relevance function to fit frame
  absolute_max <- max(max(plot_data$sample_dist), max(plot_data$amelia_dist))
  
  max_val <- absolute_max
  min_val <- 0
  
  plot_data$diff <- rescale(plot_data$diff, to = c(min_val, max_val))
  
  # Adding the plot for the relevance function. This is done in single segments, which is pretty slow.
  
  if (plot_relevance){
    for (i in 1:length(plot_data$diff)){
      plot <- plot + 
        geom_segment(aes(color = "Relevance Function"), x = i-0.5, xend = i+0.5, y = plot_data$diff[i], yend = plot_data$diff[i])
      if (i < length(plot_data$diff)) plot <- plot + geom_segment(aes(color = "Relevance Function"), x = i+0.5, xend = i+0.5, y = plot_data$diff[i], yend = plot_data$diff[i+1])
      
    }
  }
  
  color <- c("Relevance Function" = "#3634CA")
  
  
  # Mean / Median
  if (show_mean){
    plot <- plot + geom_hline(aes(yintercept = mean(plot_data$diff), color = "Mean"), lty = "dashed")
    color <- c(color, "Mean" = "#018D43")
  }   
  if (show_median){
    plot <- plot + geom_hline(aes(yintercept = median(plot_data$diff), color = "Median"), lty = "dashed")
    color <- c(color, "Median" = "#785889")
  } 
  
  plot <- plot + scale_color_manual(values = color)
  
  ### Theme ###
  
  if (!is.null(sample_type)){
    title <- sprintf("Income distribution in %s sample and true population with resulting relevance function", sample_type)
  } else {
    title <- "Income distribution in sample and true population with resulting relevance function"
  }
  
  if(plot_relevance) subtitle <- "Difference has been scaled to range between 0 and maximum density value"
  else subtitle <- NULL
  
  
  plot <- plot +
    guides(fill = guide_legend(title=NULL), color = guide_legend(title=NULL)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          axis.line = element_line(size = 0.2, colour = "black", linetype=1),
          panel.background = element_rect(fill = 'white', colour = 'white')) + 
    ggtitle(label = title,
            subtitle = subtitle) +
    labs(x = "Income", y = "Density")
  
  
  print(plot)
}
