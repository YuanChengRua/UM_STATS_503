train_data = read.csv("E:\\diabetes_train.csv")
test_data = read.csv("E:\\diabetes_test.csv")

library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(ggplot2)



# basic_eda <- function(data)
# {
#   glimpse(data)
#   print(status(data))
#   freq(data) 
#   print(profiling_num(data))
#   plot_num(data)
#   describe(data)
# }
# 
# glimpse(train_data)
# #We can see the general summary of the training data which contains 428 rows/obversations and 9 columns. Each column except the
# #last one represents a feature (X). The data type of each feature is shown and there are 7 int type and 2 double type. 
# 
# status(train_data)
# #From the table shown, we can conclude the percertanges of zeros, N/A, inf. And the table shows that there are 223 0s, which correspond
# #52.1% of total outcome. The zeros represents the cases of not having diabetes
# 
# freq(train_data$Outcome)#To view the percertage of the outcomes 
# 
# 
# #To check the numeric data 
# plot_num(train_data$Pregnancies)
# 
# train_data_prof=profiling_num(train_data)
# #Check the mean, std_dev of each variable
# 
# df = train_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")]
# pairs(df)
# #Check the relationship between each pair of two variables.
# library(GGally)
# ggpairs(df, title="correlogram with ggpairs()") 
# #Check the correlations between each pair of two variables.



library(FNN)

x_train = train_data[,1:8]
y_train = train_data$Outcome
x_test = test_data[,1:8]
y_test = test_data$Outcome

scaled_x_train = scale(x_train)
scaled_x_test = scale(x_test)




k = 1:50
test_error <- rep(x = 0, times = length(k))
train_error <- rep(x = 0, times = length(k))

for (i in seq_along(k)) {
  pred <- knn(train = scaled_x_train, test  = scaled_x_test, cl = y_train, k = k[i])
  test_error[i] <- mean(y_test != pred)
}

for (j in seq_along(k)){
  pred <- knn(train=scaled_x_train, test = scaled_x_train, cl = y_train, k = k[j])
  train_error[j] <- mean(y_train != pred)
}
complexity = rep(x = 0, times = length(k))
for (i in seq_along(k)){
  complexity[i] = 1/i
}


matplot(cbind(test_error,train_error),type="b",col=c("red","green"),lty=c(1,1))
