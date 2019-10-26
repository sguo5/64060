library(caret)
library(ISLR)
library(ggplot2)
library(dplyr)
library(FNN)
library(PreProcess)
library(oompaBase)
library(gmodels)
library(e1071)
setwd("~/Desktop")
UB <- read.csv("UniversalBank.csv")

summary(UB)
UB <- subset(UB, select = -c(1, 5))
UB$Education <- as.factor(UB$Education)
ED <- dummyVars(~Education, data = UB)
levels(ED)
head(Bank)
set.seed(123)
Test_Index <- createDataPartition(UB$Age,p=0.6,list=FALSE) 
Train_Data <- UB[Test_Index,]
Validation_Data <- UB[-Test_Index,]
summary(Train_Data)
summary(Validation_Data)

norm.values <- preProcess(Train_Data, method= c("center", "scale"))
train.norm.df <- predict(norm.values, Train_Data) 
valid.norm.df <- predict(norm.values, Validation_Data)
test.norm.df <- predict(norm.values, Test_Data)
summary(train.norm.df)
var(train.norm.df)
summary(valid.norm.df)
var(valid.norm.df)

kn <- knn(train = train.norm.df, test = test.norm.df, cl = train.norm.df[,3], k=3, prob=TRUE)
row.names(Train_Data)[attr(kn, "nn.index")]          
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
for(i in 1:14) 
kn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], cl = train.norm.df[, 3], k = i)
accuracy.df[i, 2] <- confusionMatrix(kn.pred, valid.norm.df[, 3])$overall[1] 
accuracy.df
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
norm.values <- preProcess(TraVal_Data[, 1:2], method=c("center", "scale")) 
traval.norm.df[, 1:2] <- predict(norm.values, TraVal_Data[, 1:2])
test.norm.df[, 1:2] <- predict(norm.values, Test_Data[, 1:2])
summary(traval.norm.df)
summary(test.norm.df)

kn.pred.new <- knn(traval.norm.df[, 1:2], test.norm.df, cl = traval.norm.df[, 3], k = 9)
row.names(TraVal_Data)[attr(kn, "nn.index")]
norm_model <- preProcess(UB, method = c('range'))
Df_normalized <- predict(norm_model,UB)
summary(Df_normalized)
sd(Df_normalized$balance)

norm_model <- PreProcess(Default, method = c('range'))
Df_normalized <- predict(norm_model,Default)
Index_Train <- createDataPartition(Df_normalized$default, p=0.6, list=FALSE)
Train <- Df_normalized[Index_Train,]
Test <- Df_normalized[-Index_Train,]
Train_Predictors <- Train[,2:3] 
Test_Predictors <- Test[,2:3]
Train_labels <- Train[,1] 
Test_labels <- Test[,1] 
Predicted_Test_labels <- knn(Train_Predictors, Test_Predictors, cl=Train_labels, k=4)
class_prob <- attr(Predicted_Test_labels, 'prob')
head(class_prob)
