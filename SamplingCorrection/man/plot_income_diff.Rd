% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_income_diff.R
\name{plot_income_diff}
\alias{plot_income_diff}
\title{Plot difference between sample data income and FULL Amelia income with resulting relevance}
\usage{
plot_income_diff(
  sample_data,
  amelia,
  show_mean,
  show_median,
  sample_type,
  plot_relevance
)
}
\arguments{
\item{sample_data}{Output value of compute_income_diff()}

\item{amelia}{Income distribution of original AMELIA dataset with bucket size}

\item{show_mean}{Whether mean should be plotted as a horizontal line}

\item{show_median}{Whether median should be plotted as a horizontal line}

\item{sample_type}{Implemented sampling method, gets added to the title if not null}

\item{padding}{By how much the relevance function has been padded (i.e. by how much the min/max differ from [0,1])}
}
\value{
Histogram of density of distributions + plot of difference+
}
\description{
This function is used to show the difference in income between the sample and the
actual distribution in the AMELIA dataset.
}
