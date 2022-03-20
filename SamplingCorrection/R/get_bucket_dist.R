#' Get bucketed income distribution of a sample for a provided set of income buckets
#'
#' @description This function computes the buckets for a given income sample, which is needed to compare it
#' to the known information about the income distribution of the full dataset.
#'
#' @param x income of sample
#' @param buckets vector of buckets based on AMELIA dataset
#'
#' @returns a list with amount people per income class (table) and  the proportion of people per income class (prop.table)
#'
#' @export

get_bucket_dist <- function(x, buckets) {
  dist_val <- table(cut(x, breaks = buckets, include.lowest = T))
  dist_prop <- prop.table(dist_val)

  return(list("dist_val" = dist_val, "dist_prop" = dist_prop))
}