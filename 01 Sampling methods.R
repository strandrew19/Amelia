require(dplyr)
require(feather)
require(splitstackshape)

Amelia <- read_feather("Amelia.feather")

#Set Sample Size
N <- 100000

#Set Seed
set.seed(12345)

#Simple Random Sampling without replacement
SRSWOR <- sample_n(Amelia, N)

# Proportional Size of Strata (Assume provided Register Data)
# Calculated from Dataframe with proportions below.
# Ensure that 10,000 samples are drawn from every sampling method 
# (0.01834, 0.07343, 0.15315, 0.14625, 0.11904, 0.03391
#  0.09763, 0.08739, 0.07358, 0.12563, 0.07165)

# Strata draws for total size of 10,000
Strata <- c("1" = 1834, "2" = 7343, "3" = 15315, "4" =14625, 
            "5" = 11904, "6" = 3391, "7" = 9763, "8" =8739, 
            "9" = 7358, "10" = 12563, "11" = 7165)

#Stratified Random Sampling without replacement
SSRSWOR <- stratified(Amelia, "Province", size = Strata)

#Draw Largest Cluster in each stratum for total size of 10,000
Clusters <- c("4", "5", "10", "12", "19", "23", "27", "29", 
              "34", "38", "40")

#Cluster Sampling without replacement
CSWOR <- stratified(Amelia, c("Province"), 
                    Strata, select= list(District = Clusters))
