library(InformationValue)
library(caret)

#import data
data <- read.csv("train_set.csv")

head(inputData)

#preprocess the data
str(data)

#change the catogrical variable to factors
data$default = factor(data$default,levels = c('no', 'yes'),labels = c(0,1))
data$loan = factor(data$loan,levels = c('no', 'yes'),labels = c(0,1))
data$contact = factor(data$contact,levels = c('unknown', 'cellular','telephone'),labels = c(0,1,2))
data$poutcome = factor(data$poutcome,levels = c('unknown', 'other','failure','success'),labels = c(0,1,2,3))
data$marital = factor(data$marital,levels = c('married', 'divorced','single'),labels = c(0,1,2))
data$education = factor(data$education,levels = c('tertiary', 'primary','secondary','unknown'),labels = c(0,1,2,3))
data$housing = factor(data$housing,levels =c('no', 'yes'),labels = c(0,1))
data$month = as.factor(data$month)
data$job = as.factor(data$job)

#check the result of type change
str(data)

#split the data
trainList <- createDataPartition(data$y,p = 0.8,list = FALSE)
trainset <- data[trainList,]
testset <- data[-trainList,]

#use glm to perform logistic regression
logistic <- glm(as.factor(y)~age+marital+education+default
                +balance+housing+loan+contact+day+duration
                +campaign+pdays+previous+poutcome,
                data = trainset, family= "binomial")
summary(logistic)

#trying to get the confusion matrix
predicted <- plogis(predict(logistic, testset))  

library(InformationValue)
library(caret)
optCutOff <- optimalCutoff(testset$y, predicted)
Example<-confusionMatrix(testset$y, predicted, threshold = optCutOff)

#show the result
Example

accu<-(4313+272)/(4313+319+159+272)
sens<-sensitivity(testset$y, predicted)
spec<-specificity(testset$y, predicted)
blanc<-(sens+spec)/2
sens
sepc
blanc
