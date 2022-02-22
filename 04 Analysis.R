library(tidyverse)
library()

scores_long <- pivot_longer(scores, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")
holdout_long <- pivot_longer(scores_holdout, cols = -X, names_to = c("Regression", "Sample", "Correction"), names_sep = "_")

scores_long$sample <- "test"
holdout_long$sample <- "holdout"

long <- rbind(scores_long, holdout_long)

ggplot(long, aes(x = Sample, y = value, fill = Correction))+
  geom_boxplot() +
  facet_wrap(~Regression + sample) +
  jtools::theme_apa()
