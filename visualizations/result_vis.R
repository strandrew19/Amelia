*Stranberg, Andrew*
  *Schischke, David*
  *Nazarova, Ekaterina*
  
  
  ```{r, libraries}
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggpubr)
```


Load scores_holdout.csv
```{r, holdout}
#results_holdout = read.csv("/Users/ekaterinanazarova/Desktop/UniTrier/5th semester - the last one/Research Case Study/scores_holdout.csv",
#sep = ";")
"your path"
# rename first column
results_holdout = results_holdout %>%
  rename(iteration = X)
```

Reshape the table
```{r, holdout}
results_holdoutTransformed = pivot_longer(results_holdout, cols = c("RF_SRS_Base","RF_SRS_Importance","RF_SRS_Synthetic",                 "RF_Stratified_Base", "RF_Stratified_Importance","RF_Stratified_Synthetic","RF_Stratified.Cluster_Base",         "RF_Stratified.Cluster_Importance","RF_Stratified.Cluster_Synthetic","MLPR_SRS_Base","MLPR_SRS_Importance",               
                                                                    "MLPR_SRS_Synthetic","MLPR_Stratified_Base","MLPR_Stratified_Importance","MLPR_Stratified_Synthetic","MLPR_Stratified.Cluster_Base",       "MLPR_Stratified.Cluster_Importance","MLPR_Stratified.Cluster_Synthetic","LR_SRS_Base","LR_SRS_Importance",                 
                                                                    "LR_SRS_Synthetic","LR_Stratified_Base","LR_Stratified_Importance","LR_Stratified_Synthetic","LR_Stratified.Cluster_Base",        "LR_Stratified.Cluster_Importance","LR_Stratified.Cluster_Synthetic"),
                                          names_to = "method")
results_holdoutTransformed = separate(results_holdoutTransformed, col = method, into = c("model", "sampling", "correction"), sep = '_')
```

ggplot with Plotly
```{r, holdout}
a=results_holdoutTransformed %>%
  filter(model == "RF") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Random Forest")
ggplotly(a)
a=results_holdoutTransformed %>%
  filter(model == "LR") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Linear regression")
ggplotly(a)
a=results_holdoutTransformed %>%
  filter(model == "MLPR") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Neural Net")
ggplotly(a)
```


Load scores.xlsx (test dataset). The problem with this dataset (at least for my version of Rstudio) that it didnt work neither with read.csv nore with read_exce in a way to reshape the data and separate it (maybe smth happened during transformation pages to excel).

Additionally, \for loop\ or \function\ didnt work out either to execute *as.numeric(gsub('"', '', data))* for each column. Instead of numeric values *r* converted it to *NA*. Thus, each single column was transformed by hands separately.
```{r, test}
#scores = read_excel("Desktop/UniTrier/5th semester - the last one/Research Case Study/scores.xlsx")
"your path"
# rename columns and delete the first row
colnames(scores) = scores[1,]
scores=scores[-1,]
# Transform it to numeric
scores$RF_SRS_Base = as.numeric(gsub('"', '', scores$RF_SRS_Base)) 
scores$RF_SRS_Importance = as.numeric(gsub('"', '', scores$RF_SRS_Importance)) 
scores$RF_SRS_Synthetic = as.numeric(gsub('"', '', scores$RF_SRS_Synthetic)) 
scores$RF_Stratified_Base = as.numeric(gsub('"', '', scores$RF_Stratified_Base)) 
scores$RF_Stratified_Importance = as.numeric(gsub('"', '', scores$RF_Stratified_Importance))
scores$RF_Stratified_Synthetic = as.numeric(gsub('"', '', scores$RF_Stratified_Synthetic))
scores$`RF_Stratified Cluster_Base` = as.numeric(gsub('"', '', scores$`RF_Stratified Cluster_Base`))
scores$`RF_Stratified Cluster_Importance` = as.numeric(gsub('"', '', scores$`RF_Stratified Cluster_Importance`))
scores$`RF_Stratified Cluster_Synthetic` = as.numeric(gsub('"', '', scores$`RF_Stratified Cluster_Synthetic`))
scores$MLPR_SRS_Base = as.numeric(gsub('"', '', scores$MLPR_SRS_Base))
scores$MLPR_SRS_Importance = as.numeric(gsub('"', '', scores$MLPR_SRS_Importance))
scores$MLPR_SRS_Synthetic = as.numeric(gsub('"', '', scores$MLPR_SRS_Synthetic))
scores$MLPR_Stratified_Base = as.numeric(gsub('"', '', scores$MLPR_Stratified_Base))
scores$MLPR_Stratified_Importance = as.numeric(gsub('"', '', scores$MLPR_Stratified_Importance))
scores$MLPR_Stratified_Synthetic = as.numeric(gsub('"', '', scores$MLPR_Stratified_Synthetic))
scores$`MLPR_Stratified Cluster_Base` = as.numeric(gsub('"', '', scores$`MLPR_Stratified Cluster_Base`))
scores$`MLPR_Stratified Cluster_Importance` = as.numeric(gsub('"', '', scores$`MLPR_Stratified Cluster_Importance`))
scores$`MLPR_Stratified Cluster_Synthetic` = as.numeric(gsub('"', '', scores$`MLPR_Stratified Cluster_Synthetic`))
scores$LR_SRS_Base = as.numeric(gsub('"', '', scores$LR_SRS_Base))
scores$LR_SRS_Importance = as.numeric(gsub('"', '', scores$LR_SRS_Importance))
scores$LR_SRS_Synthetic = as.numeric(gsub('"', '', scores$LR_SRS_Synthetic))
scores$LR_Stratified_Base = as.numeric(gsub('"', '', scores$LR_Stratified_Base))
scores$LR_Stratified_Importance = as.numeric(gsub('"', '', scores$LR_Stratified_Importance))
scores$LR_Stratified_Synthetic = as.numeric(gsub('"', '', scores$LR_Stratified_Synthetic))
scores$`LR_Stratified Cluster_Base` = as.numeric(gsub('"', '', scores$`LR_Stratified Cluster_Base`))
scores$`LR_Stratified Cluster_Importance` = as.numeric(gsub('"', '', scores$`LR_Stratified Cluster_Importance`))
scores$`LR_Stratified Cluster_Synthetic` = as.numeric(gsub('"', '', scores$`LR_Stratified Cluster_Synthetic`))
# Reshape the table
results_testTransformed = pivot_longer(scores, cols = c("RF_SRS_Base","RF_SRS_Importance","RF_SRS_Synthetic",                 "RF_Stratified_Base", "RF_Stratified_Importance","RF_Stratified_Synthetic","RF_Stratified Cluster_Base",         "RF_Stratified Cluster_Importance","RF_Stratified Cluster_Synthetic","MLPR_SRS_Base","MLPR_SRS_Importance",               
                                                        "MLPR_SRS_Synthetic","MLPR_Stratified_Base","MLPR_Stratified_Importance","MLPR_Stratified_Synthetic","MLPR_Stratified Cluster_Base",       "MLPR_Stratified Cluster_Importance","MLPR_Stratified Cluster_Synthetic","LR_SRS_Base","LR_SRS_Importance",                 
                                                        "LR_SRS_Synthetic","LR_Stratified_Base","LR_Stratified_Importance","LR_Stratified_Synthetic","LR_Stratified Cluster_Base",        "LR_Stratified Cluster_Importance","LR_Stratified Cluster_Synthetic"),
                                       names_to = "method")
results_testTransformed$method = gsub(" ", ".", results_testTransformed$method)                                             
results_testTransformed = separate(results_testTransformed, col = method, into = c("model", "sampling", "correction"), sep = '_')
colnames(results_testTransformed)[1] = "iteration"
```

ggplot with Plotly
```{r, test}
a=results_testTransformed %>%
  filter(model == "RF") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Random Forest")
ggplotly(a)
a=results_testTransformed %>%
  filter(model == "LR") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Linear regression")
ggplotly(a)
a=results_testTransformed %>%
  filter(model == "MLPR") %>%
  group_by(sampling, correction) %>%
  ggplot(aes(y = value, x = sampling)) +
  geom_boxplot(aes(fill = correction)) +
  geom_jitter(aes(color = as.factor(iteration)))+
  ggtitle("Neural Net")
ggplotly(a)
```