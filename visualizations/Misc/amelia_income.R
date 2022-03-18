wd <- sprintf("%s/..", dirname(rstudioapi::getSourceEditorContext()$path))
setwd(wd)

library(arrow)
library(dplyr)
library(stringr)
devtools::load_all("functions/SamplingCorrection")

amelia_full_income <- data.frame(read_feather(sprintf("%s/data/AMELIA.feather", wd))$Person_Income)
colnames(amelia_full_income) <- c("income")

# Set bucket size
# Source: https://unstats.un.org/unsd/demographic/sources/census/quest/NZL2013enIn.pdf
BUCKETS <- c(seq(0, 40000, by = 5000), seq(50000, 70000, by = 10000), 100000, 150000, max(amelia_full_income)+1)


ggplot(data = amelia_full_income, aes(x = income)) + 
  geom_histogram(aes(y=..density..), breaks = BUCKETS)

props <- as.character(as.numeric(round(prop.table(table(cut(amelia_full_income$income, breaks = BUCKETS, include.lowest = T))),4)))

# Print for latex 

paste(props, collapse = " & ")
