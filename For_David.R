####################
"DATA PREPROCESSING"
####################


# 1. Convert to numeric
srs$Sex = as.numeric(as.factor(unique(srs$Sex)))

# 2. Scale data
srs = srs %>%
  relocate(Person_Income) # move Y column to first position

srs[,-1] = scale(srs[,-1], center = T) # scale everything except first column

####################
"TUNING PART"
####################

"\\RANDOM FOREST"
et.seed(123)
control = trainControl(method="repeatedcv", 
                       number=10, 
                       repeats=5, 
                       savePredictions = T,
                       search = "grid")

tunegrid = expand.grid(.mtry=c(1:15))

rf_gridsearch = train(Person_Income~., 
                      data = srs,
                      method = "rf", 
                      tuneGrid=tunegrid,
                      trControl=control,
                      na.action = na.omit,
                      ntree = 1000,
                      metric = "RMSE",
                      importance = TRUE)
rf_gridsearch
"You should have smth like [the best rmty = ... regarding the lowest RMSE] "


"\\NEURAL NETWORK"

#install.packages("NeuralNetTools")
#install.packages("NeuralSens")
library(NeuralNetTools) 
library(NeuralSens)

"Try both approaches and chose the best one (RMSE is lower or R2 is higher)"
"1"
NN_control = trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats=3)


neural_net = train(Person_Income~.,
                   data = train,
                   method = 'nnet', 
                   trControl = NN_control,
                   linear.output = T, 
                   algorithm = "backprob", 
                   metric = "RMSE",
                   maxit = 250,
                   tuneLength = 10)
neural_net  
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 19 and decay = 0.007498942.

"2"
NN_control_2 = trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats=3)


neural_net_2 = train(Person_Income~.,
                     data = train,
                     method = 'nnet', 
                     trControl = NN_control,
                     linear.output = T, 
                     algorithm = "backprob", 
                     metric = "RMSE",
                     maxit = 250,
                     tuneGrid=expand.grid(size=c(2:25),
                                          decay=10^seq(-9,0,by=1)))

neural_net_2 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 15 and decay = 0.01.


