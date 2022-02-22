library(tidyverse)

wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

scores <- read.csv(sprintf("%s/results/scores.csv", wd))
scores_holdout <- read.csv(sprintf("%s/results/scores_holdout.csv", wd))

scores_long <- pivot_longer(scores, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")
holdout_long <- pivot_longer(scores_holdout, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")

scores_long$sample <- "test"
holdout_long$sample <- "holdout"

long <- rbind(scores_long, holdout_long)
long$Regression <- tolower(long$Regression)
long$Sample <- tolower(long$Sample)
long$Correction <- tolower(long$Correction)

ggplot(long, aes(x = Sample, y = value, fill = Correction))+
  geom_boxplot() +
  facet_wrap(~ sample + Regression) +
  jtools::theme_apa()
