rm(list = ls())
require(arrow)
require(dplyr)
library(sjmisc)
require(caTools)

"Change WD (Assigned to reset for holdout_dir assignment"
reset<- setwd("c:\\Users\\Andrew\\Documents\\Uni Trier\\Semester 3\\Case Study\\ML Algo")
reset

top_wd <- getwd()
feather_dir <- paste(top_wd, "/Feather", sep = "")
results_dir <- paste(top_wd, "/Results", sep = "")
setwd('..')
holdout_dir <- paste(getwd(), "/AMELIA/AMELIA_P_level_v0.2.3 (Person-Level)", sep = "") 
Holdout <- arrow::read_feather(paste(holdout_dir, "/Holdout.feather", sep = ""))

"Drop Uncessary Columns"
drop_cols <- function(dataframe){dplyr::select(dataframe, -c('index', 'Personal_ID', 'City/Community'))}

"Convert Sex variables to numeric"
to_numeric <- function(dataframe){as.numeric(factor(dataframe$Sex))}

"Create Dummy Variables"
cata_encode <- function(dataframe){
  
  tmp <- dataframe %>%
    sjmisc::to_dummy(Work_Status) %>%
    dplyr::select(1:3)
  colnames(tmp) <- c("At Work","Unemployed","Retired")
  
  dataframe <- dataframe %>%
    dplyr::select(-c("Work_Status")) %>%
    cbind(tmp)
  
  tmp <- dataframe %>%
    sjmisc::to_dummy(Highest_ISCED) %>%
    dplyr::select(1:4)
  colnames(tmp) <- c("ISCED 1","ISCED 2","ISCED 3","ISCED 4")
  
  dataframe <- dataframe %>%
    dplyr::select(-c("Highest_ISCED")) %>%
    cbind(tmp)
  
  tmp <- dataframe %>%
    sjmisc::to_dummy(Martial_Status) %>%
    dplyr::select(1:4)
  colnames(tmp) <- c("Never Married", "Married", "Separated", "Widowed")
  
  dataframe <- dataframe %>%
    dplyr::select(-c("Martial_Status")) %>%
    cbind(tmp)
  
  tmp <- dataframe %>%
    sjmisc::to_dummy(Regional_ID) %>%
    dplyr::select(1:3)
  colnames(tmp) <- c("Region_1", "Region_2", "Region_3")
  
  dataframe <- dataframe %>%
    dplyr::select(-c("Regional_ID")) %>%
    cbind(tmp)
}

"Train Test Split (and scaling x vars)"
t_t_split <- function(dataframe){
  "Split into y and x vars"
  y <- dataframe$Person_Income
  x <- dataframe %>% dplyr::select(-c(Person_Income))
  y <- data_frame(y)
  "Split data and save unscaled x data for later comparison"
  set.seed(420) #Make Results Reproducable
  x_train_unscaled <- sample_frac(x, 0.75)
  x_train_scaled <- scale(x_train_unscaled)
  set.seed(420) #Make Results Reproducable
  y_train <- sample_frac(y, 0.75)
  
  tmp_x <- as.numeric(rownames(x_train_unscaled)) # because rownames() returns character
  tmp_y <- as.numeric(rownames(y_train))
  
  "Store the test dataset for x and y"
  x_test_unscaled<-x[-tmp_x,]
  x_test_scaled <- scale(x_test_unscaled)
  y_test <- y[-tmp_y,]
  
  return(list(y_train, y_test, x_train_scaled, x_train_unscaled, x_test_scaled, x_test_unscaled))
}

Holdout <- drop_cols(Holdout)
Holdout$Sex <- to_numeric(Holdout)
Holdout <- Holdout %>% select(Person_Income, everything())
Holdout <- cata_encode(Holdout)


#test
"Returns:y_train, y_test, x_train_scaled, x_train_unscaled, x_test_scaled, x_test_unscaled"
tmp <- t_t_split(Holdout)
y_t <- tmp[[1]]
x_t <- tmp[[3]]
x_t <- data_frame(x_t)

regr <- lm(y_t$y ~ ., data = x_t)
results <- summary(regr)
results$r.squared
