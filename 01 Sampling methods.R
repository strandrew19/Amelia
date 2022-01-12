require(dplyr)
require(arrow)
require(splitstackshape)
library(stringr)

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)
Amelia <- read_feather("data/Amelia.feather")

# Remove any values that contain NA (important for sample correction methods)
pre <- nrow(Amelia)
Amelia <- Amelia[complete.cases(Amelia), ]
print(sprintf("Removed %d rows containing NA values", pre - nrow(Amelia)))

# Convert strings to factors (important for sample correction methods)
Amelia <- as_tibble(unclass(Amelia), stringsAsFactors = T)

#Set Sample Size
N <- 100000

# Proportional Size of Strata (Assume provided Register Data)
# Calculated from Dataframe with proportions below.
# Ensure that 10,000 samples are drawn from every sampling method 
# (0.01834, 0.07343, 0.15315, 0.14625, 0.11904, 0.03391
#  0.09763, 0.08739, 0.07358, 0.12563, 0.07165)

# Strata draws for total size of 10,000
Strata <- c("1" = 1834, "2" = 7343, "3" = 15315, "4" =14625, 
            "5" = 11904, "6" = 3391, "7" = 9763, "8" =8739, 
            "9" = 7358, "10" = 12563, "11" = 7165)

#Draw Largest Cluster in each stratum for total size of 10,000
Clusters <- c("4", "5", "10", "12", "19", "23", "27", "29", 
              "34", "38", "40")

# Monte Carlo Simulation part

for (i in 1:10){
  print(sprintf("Starting run %d", i))
  #Set Seed
  set.seed(i)
  
  #Simple Random Sampling without replacement
  SRSWOR <- data.frame(sample_n(Amelia, N)) # data.frame format required for sample correction functions
  
  #Stratified Random Sampling without replacement
  SSRSWOR <- data.frame(stratified(Amelia, "Province", size = Strata))
  
  #Cluster Sampling without replacement
  CSWOR <- data.frame(stratified(Amelia, c("Province"), 
                                 Strata, select= list(District = Clusters)))
  
  out <- list("SRS" = list("Base" = SRSWOR), "Stratified" = list("Base" = SSRSWOR), "Stratified Cluster" = list("Base" = CSWOR))
  
  filename <- sprintf("data/samples/%s_sample.rds", str_pad(i, 2, pad = "0"))
  saveRDS(out, filename)
}