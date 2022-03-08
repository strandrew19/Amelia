wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

library(arrow)
library(stringr)
devtools::load_all(sprintf("%s/functions/SamplingCorrection",wd))


BUCKETS <- c(seq(0, 40000, by = 5000), seq(50000, 70000, by = 10000), 100000, 150000, 500000)
holdout <- read_feather(sprintf("%s/data/Holdout.feather",wd))$Person_Income
holdout_bucket <- get_bucket_dist(holdout$Person_Income, BUCKETS)


iter <- str_pad(1:2, width = 2, pad = "0")
sample <- c("SRS", "Stratified", "Stratified Cluster")
correction <- c("base", "importance", "synthetic")
regr <- c("LR", "RF", "MLPR")



for (i in iter){
  for (s in sample){
    for (c in correction){
      for (r in regr){
        varname <- ifelse(r == "LR", "predict_vals", "Predictions")
        pred <- read_feather(sprintf("%s/results/%s_%s_%s_%s_holdout_results.feather", wd, i, s, c, r))[varname]
      }
    }
  }
}

