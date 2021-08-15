# remove the objects stored
rm(list=ls())

#setting working directory
setwd("G:/Edwiser material/Project/Santandarcustomer problems/Edwiser project")

#checking the set directory
getwd()

# Loading required Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE", 'sampling',
      'DataCombine', 'inTrees','dplyr','class','scales', "InformationValue",
      "RDocumentation", 'mlbench')

#Installing packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Loading the both train and test data
train = read.csv("train.csv")
test = read.csv("test.csv")

# checking for the dimensions of the data
dim(train)
dim(test)
str(train)
str(test)
head(train,4)
head(test,4)

#Removing the ID_code column  which is nothing but the code and does not have information
train$ID_code=NULL
test$ID_code=NULL

#converting the target variable from numeric to factor
train$target=as.factor(train$target)

#Selecting all the numeric variables in train data
numeric_index=sapply(train, is.numeric)
numeric_data=train[,numeric_index]
cnames=colnames(numeric_data)

#Removing all the objects except the required ones
rmExcept(c("train","test","cnames"))

####################Sampling########################### 
trainDataIndex = createDataPartition(train$target, p=0.7, list = F)
train = train[trainDataIndex, ]
test = train[-trainDataIndex, ]

# Class distribution of train data
table(train$target)

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

#Down Sampling
set.seed(100)
train = downSample(x = train[, colnames(train) %ni% "target"],
                         y = train$target)

table(train$Class)

#################MOdel development##############
######Random Forest Model########
Model_RF=randomForest(Class~., train, importance=TRUE, ntree=50)

#Summary of the RF model
summary(Model_RF)

##Extracting rules from the trained random forest model
#transforming random forest object to intrees format
treeList=RF2List(Model_RF)

#Extracting rules
exec=extractRules(treeList, train[,-1])

#Visualizing few rules
exec[1:2,]

#Makinig rules more readable
readableRules=presentRules(exec, colnames(train))

readableRules[1:2,]

#Getting rule matrix
ruleMetrics=getRuleMetric(exec, train[,-1], train$target)

#Evaluating the rules
ruleMetrics[1:2,]

#Predicting the test target variable class values with trained random forest model
Predictions_RF = predict(Model_RF, test[,-1])

#Evaluating the performence of the Random forest model
confMatrix_RF = table(test$target, Predictions_RF)

confMatrix_RF

#plotting ROc curve to RF predictions
pred=as.numeric(Predictions_RF)

plotROC(test$target, pred)

AUROC(test$target, pred)

#precision calculation
precision(test$target, pred)

pred=as.factor(Predictions_RF)

recall(test$target, pred)

#Accuarcy:
#FNR: