rm(list = ls())
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

library(tidyverse)
library(latex2exp)
library(arrow)
devtools::load_all(sprintf("%s/SamplingCorrection",wd))


##### Prep ######

scores <- read.csv(sprintf("%s/results/scores.csv", wd), sep = ";")
scores_holdout <- read.csv(sprintf("%s/results/scores_holdout.csv", wd), sep = ";")

scores_long <- pivot_longer(scores, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")
holdout_long <- pivot_longer(scores_holdout, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")

scores_long$sample <- "test"
holdout_long$sample <- "holdout"

long <- rbind(scores_long, holdout_long)

long$X <- factor(long$X)
long$Regression <- factor(long$Regression, labels = c("LR", "NN", "RF"))
long$Regression <- ordered(long$Regression, levels = c("LR", "RF", "NN"))
long$Sample <- factor(long$Sample, labels = c("SRS", "StRS", "StCRS"))
long$Correction <- factor(tolower(long$Correction), labels = c("No correction", "Importance", "Synthetic"))
long$sample <- factor(long$sample, labels = c("Out-of-Sample", "In-Sample"))
long$sample <- ordered(long$sample, levels = c("In-Sample", "Out-of-Sample"))

##### Regression-wise comparison ##### 

reg_plot_corr <- function(method, oos = F){
  p <- ggplot(filter(long, Regression == method, sample == ifelse(oos, "Out-of-Sample", "In-Sample")), aes(x = Sample, y = value, fill = Correction))+
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.3, pch = 19) + 
    theme_minimal() + 
    ggsci::scale_fill_tron() +
    guides(fill = guide_legend(title = "Correction method")) + 
    xlab("Sampling method") + 
    ylab(TeX("$R^2$")) + 
    ylim(c(min(filter(long, Regression == method)$value), max(filter(long, Regression == method)$value)))
  
  fname <- ifelse(oos, sprintf("%s_OOS.png", method), sprintf("%s_IS.png", method))
  ggsave(sprintf("%s/visualizations/results graphics/regression/%s", wd, fname), plot = p, width = 12, height = 7)
}

for (method in c("LR", "RF", "NN")){
  for (oos in c(T, F)){
    reg_plot_corr(method, oos)
  }
}

##### Sampling-wise comparison #####   

samp_plot_base <- function(samp, oos = F){
  p <- ggplot(filter(long, Sample == samp, sample == ifelse(oos, "Out-of-Sample", "In-Sample"), Correction == "No correction"), aes(x = Regression, y = value, fill = Regression))+
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.3, pch = 19) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    ggsci::scale_fill_tron() +
    xlab("Regression method") + 
    ylab(TeX("$R^2$")) + 
    ylim(c(0.08, 0.42))
  
  fname <- ifelse(oos, sprintf("%s_OOS_nocorr.png", samp), sprintf("%s_IS_nocorr.png", samp))
  ggsave(sprintf("%s/visualizations/results graphics/sampling/%s", wd, fname), plot = p, width = 6, height = 5)
}

for (method in c("SRS", "StRS", "StCRS")){
  for (oos in c(T, F)){
    samp_plot_base(method, oos)
  }
}

##### Comparison of predicted to true mean ###### 

BUCKETS <- c(seq(0, 40000, by = 5000), seq(50000, 70000, by = 10000), 100000, 150000, 500000)
holdout <- read_feather(sprintf("%s/data/Holdout.feather",wd))$Person_Income
holdout_bucket <- get_bucket_dist(holdout, BUCKETS)
holdout_mean <- mean(holdout)
holdout_sd <- sd(holdout)

iter <- str_pad(1:10, width = 2, pad = "0")
sample <- c("SRS", "Stratified", "Stratified Cluster")
correction <- c("base", "importance", "synthetic")
regr <- c("LR", "RF", "MLPR")

vals <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(vals) <- c("iter", "sample", "correction", "regr", "value", "sd")



for (i in iter){
  for (s in sample){
    for (c in correction){
      for (r in regr){
        varname <- ifelse(r == "LR", "predict_vals", "Predictions")
        pred <- read_feather(sprintf("%s/results/%s_%s_%s_%s_holdout_results.feather", wd, i, s, c, r))[[varname]]
        
        temp <- data.frame(iter = i, sample = s, correction = c, regression = r, value = mean(pred), sd = sd(pred))
        vals <- rbind(vals, temp)
      }
    }
  }
}

vals$regression <- factor(vals$regression, labels = c("LR", "NN", "RF"))
vals$correction <- factor(vals$correction, labels = c("No correction", "Importance", "Synthetic"))
vals$sample <- factor(vals$sample, labels = c("SRS", "StRS", "StCRS"))

p <- ggplot(vals, aes(x = iter, y = value, col = sample, group = sample)) + 
  geom_point(size = 2) + geom_line(size = 1) + 
  #geom_errorbar(aes(ymin = value - sd, ymax = value + sd, alpha = 0.1)) + 
  xlab("Iteration") + 
  ylab("Predicted Mean Income in Out-of-sample prediction") + 
  facet_wrap(~regression + correction) +
  geom_hline(aes(yintercept = holdout_mean, col = "True holdout\nmean"), size = 1) +
  ggsci::scale_color_tron() + 
  theme_minimal()

ggsave(sprintf("%s/visualizations/results graphics/holdout_mean.png", wd), plot = p, width = 12, height = 8)

p <- ggplot(vals, aes(x = iter, y = sd, col = sample, group = sample)) + 
  geom_point(size = 2) + geom_line(size = 1) + 
  #geom_errorbar(aes(ymin = value - sd, ymax = value + sd, alpha = 0.1)) + 
  xlab("Iteration") + 
  ylab("Predicted SD of Income in Out-of-sample prediction") + 
  facet_wrap(~regression + correction) +
  geom_hline(aes(yintercept = holdout_sd, col = "True holdout\nmean"), size = 1) +
  ggsci::scale_color_tron() + 
  theme_minimal()

ggsave(sprintf("%s/visualizations/results graphics/holdout_sd.png", wd), plot = p, width = 12, height = 8)

#### Additional relevant values for Appendix

long %>% 
  filter(sample == "In-Sample") %>%
  pivot_wider(names_from = X, values_from = value) %>%
  select(-sample) %>% 
  mutate_if(is.numeric, function(x) round(x,4)) %>%
  write.csv2(file = sprintf("%s/visualizations/IS_vals_appendix.csv", wd))

long %>% 
  filter(sample == "Out-of-Sample") %>%
  pivot_wider(names_from = X, values_from = value) %>%
  select(-sample) %>% 
  mutate_if(is.numeric, function(x) round(x,4)) %>%
  write.csv2(file = sprintf("%s/visualizations/OOS_vals_appendix.csv", wd))

p <- ggplot(long, aes(x = Sample, y = value, fill = Correction))+
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.3, pch = 19) + 
  facet_wrap(~ sample + Regression, ncol = 3) + 
  theme_minimal() + 
  ggsci::scale_fill_tron() +
  guides(fill = guide_legend(title = "Correction method")) + 
  xlab("Sampling method") + 
  ylab(TeX("$R^2$"))

ggsave(sprintf("%s/visualizations/results graphics/regression/all_models_comp.png", wd), plot = p, width = 12, height = 8)

#### Other stuff

filter(long, Regression == "LR", sample == "In-Sample", Sample == "StRS", Correction == "Synthetic")
filter(long, Regression == "LR", sample == "In-Sample", Sample == "SRS", Correction == "Synthetic")

long %>% 
  filter(Regression == "LR", sample == "Out-of-Sample", Correction == "Synthetic") %>% 
  summarize("min" = min(value), "max" = max(value))
  

mean(filter(long, Regression == "LR", sample == "Out-of-Sample", Sample == "StCRS", Correction == "No correction")$value)
mean(filter(long, Regression == "LR", sample == "In-Sample", Sample == "StCRS", Correction == "No correction")$value)

filter(long, Regression == "NN", sample == "Out-of-Sample", Sample == "StCRS", Correction == "No correction")

mean(filter(long, Regression == "NN", sample == "Out-of-Sample", Sample == "StCRS", Correction == "No correction")$value)
mean(filter(long, Regression == "LR", sample == "Out-of-Sample", Sample == "StCRS", Correction == "No correction")$value)
mean(filter(long, Regression == "RF", sample == "Out-of-Sample", Sample == "StCRS", Correction == "No correction")$value)

min(filter(long, Regression == "RF", Correction == "No correction")$value)
max(filter(long, Regression == "RF", Correction == "No correction")$value)

mean(filter(long, Regression == "RF", Correction == "No correction", Sample == "SRS")$value)
mean(filter(long, Regression == "RF", Correction == "No correction", Sample == "StRS")$value)
mean(filter(long, Regression == "RF", Correction == "No correction", Sample == "StCRS")$value)

mean(filter(long, Regression == "NN", Correction == "Synthetic", sample == "In-Sample")$value)
mean(filter(long, Regression == "NN", Correction == "Synthetic", sample == "Out-of-Sample")$value)
