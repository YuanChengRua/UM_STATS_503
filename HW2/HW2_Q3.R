
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(GGally)
library(ggplot2)
library(ggthemes)
library(MASS)
library(klaR)
library(e1071)

#Load the data
train_data = read.csv("E:\\wine_train.csv")
test_data = read.csv("E:\\wine_test.csv")
train_data$Type = as.factor(train_data$Type)
test_data$Type = as.factor(test_data$Type)
levels(train_data$Type) = c("Type 1", "Type 2", "Type 3")
levels(test_data$Type) = c("Type 1", "Type 2", "Type 3")

status(train_data)
status(test_data)
#No missing data so no need for imputation


#Correlation between predictors
pairs(train_data[2:13], col=c("blue", "green","red")[train_data$Type], 
      pch=c(1,2,3)[train_data$Type])
par(xpd=TRUE)
legend(0.34, 0.71, as.vector(unique(train_data$Type)), 
       col=c("blue", "green", "red"), pch=1:3, cex = 0.5)

#BOXPLOT
dat.explore = gather(train_data, key = "Variable", value = "Value", -c(Type))
head(dat.explore)
ggplot(dat.explore) + geom_boxplot(aes(x = Type, y = Value)) + facet_wrap(.~Variable, scales = "free_y") + theme_minimal()

#Fit the training data into the lda function 

lda_model = lda(train_data$Type ~ ., data = train_data)
lda_prediction_test = predict(lda_model, test_data)
lda_test_error = mean(lda_prediction_test$class != test_data$Type)
lda_test_error
table(predicted = lda_prediction_test$class, actual = test_data$Type)

#Fit the training data into the qda function to build qda model
qda_model = qda(train_data$Type ~ ., data = train_data)
qda_prediction_test = predict(qda_model, test_data)
qda_test_error = mean(qda_prediction_test$class != test_data$Type)
qda_test_error
table(predicted = qda_prediction_test$class, actual = test_data$Type)

#Fit the training data into the naive bayes function to build the model
NBclassfier=naiveBayes(train_data$Type ~ ., data=train_data)
nb_prediction_test = predict(NBclassfier,newdata = test_data)
nb_test_error = mean(nb_prediction_test !=test_data$Type)
nb_test_error
table(predicted = nb_prediction_test, actual = test_data$Type)

