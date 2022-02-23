library(tidyverse)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

scores <- read.csv(sprintf("%s/results/scores.csv", wd), sep = ";")
scores_holdout <- read.csv(sprintf("%s/results/scores_holdout.csv", wd), sep = ";")


scores_long <- pivot_longer(scores, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")
holdout_long <- pivot_longer(scores_holdout, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")

scores_long$sample <- "test"
holdout_long$sample <- "holdout"

long <- rbind(scores_long, holdout_long)

ggplot(long, aes(x = Sample, y = value, fill = Correction))+
  geom_boxplot() +
  facet_wrap(~sample + Regression) +
  jtools::theme_apa()

# Quality of base prediction

ggplot(filter(long, Correction %in% c("Base", "base")), aes(x = Sample, y = value, fill = sample))+
  geom_boxplot() +
  facet_wrap(~Regression) +
  jtools::theme_apa()
