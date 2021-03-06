rm(list = ls())
require(arrow)
require(dplyr)
library(sjmisc)
library(caret)
library(stringr)

options(warn = -1)

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

"Change WD (Assigned to reset for holdout_dir assignment"
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

feather_dir <- paste(wd, "/data/feather_samples", sep = "")
results_dir <- paste(wd, "/results", sep = "")

fnames <- list.files(path = feather_dir, pattern = "\\.feather")

holdout <- read_feather(sprintf("%s/data/Holdout.feather", wd))%>% dplyr::select(-c('index', 'Personal_ID', 'City/Community'))
holdout$Sex <- factor(holdout$Sex)
holdout <- cata_encode(holdout)

scores <- read.csv(sprintf("%s/scores.csv", results_dir), sep = ";")
scores_holdout <- read.csv(sprintf("%s/scores_holdout.csv", results_dir), sep = ";")

for (f in fnames){
  
  data <- read_feather(sprintf("%s/%s", feather_dir, f)) %>%
    dplyr::select(-c('index', 'Personal_ID', 'City.Community')) %>%
    relocate(Person_Income)
  data$Sex <- factor(data$Sex)
  data <- cata_encode(data) # Dummy encoding
  
  # Train-test Split
  train <- data[1:round(0.75 * nrow(data)),]
  test <- data[(round(0.75 * nrow(data)) + 1):nrow(data), ]
  
  # Train model
  model <- train(Person_Income ~ ., data = train, method = "lm")
  
  test$predict_vals <- predict(model, test)
  test$predict_vals <- ifelse(test$predict_vals < 0, 0, test$predict_vals)
  test_r2 <- as.numeric(postResample(test$predict_vals, test$Person_Income)[2])
  
  holdout$predict_vals <- predict(model, newdata = holdout)
  holdout$predict_vals <- ifelse(holdout$predict_vals < 0, 0, holdout$predict_vals)
  holdout_r2 <- as.numeric(postResample(holdout$predict_vals, holdout$Person_Income)[2])
  
  split_name <- str_split(f, pattern = "_")[[1]]
  
  varname <- sprintf("LR_%s_%s", split_name[2], split_name[3])
  
  if (split_name[1] == "01"){
    scores[varname] <- 0
    scores_holdout[varname] <- 0
  }
  
  print(sprintf("%s: %.4f (test), %.4f (holdout)", f, test_r2, holdout_r2))
  
  scores[as.numeric(split_name[1]), varname] <- test_r2
  scores_holdout[as.numeric(split_name[1]), varname] <- holdout_r2

  relocate(holdout, predict_vals, .after = Person_Income)  
  relocate(test, predict_vals, .after = Person_Income)
  
  # Write feather results
  write_feather(test, sink = sprintf("%s/%s_%s_%s_LR_results.feather", results_dir, split_name[1], split_name[2], split_name[3]))
  write_feather(holdout, sink = sprintf("%s/%s_%s_%s_LR_Holdout_results.feather", results_dir, split_name[1], split_name[2], split_name[3]))
}

write.csv2(scores, file = sprintf("%s/scores.csv", results_dir), row.names = F)
write.csv2(scores_holdout, file = sprintf("%s/scores_holdout.csv", results_dir), row.names = F)

