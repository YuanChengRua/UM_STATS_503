library(ISLR)
summary(Weekly)

#a)
set.seed(1)
log_model = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(log_model)

#b)
log_model1 = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
summary(log_model1)

#c)
predict_prob = predict(log_model1, Weekly[1,],type = "response")
predict_prob
if (predict_prob > 0.5){
  predict_result = "Up"
} else {
  predict_result = "Down"
}
predict_result

#d)
error_count = rep(0, dim(Weekly)[1])
is_real_up = rep(0,dim(Weekly)[1])

for (i in 1:dim(Weekly)[1]){
  log_model_i = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = "binomial")
  predict_prob_i = predict.glm(log_model_i, Weekly[i, ], type="response")
  is_real_up[i] = Weekly[i, ]$Direction == "Up"
  if ((predict_prob_i > 0.5) != is_real_up[i]){
    error_count[i] = 1
  }
}
mean(error_count)






