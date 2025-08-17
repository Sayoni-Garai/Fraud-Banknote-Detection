install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("broom")
install.packages("car")
install.packages("gridExtra")

library(gridExtra)
library(car)
library(tidyverse)
library(broom)

library(e1071)
library(caTools)

library(caret)
library(randomForest)
library(MASS)
library(mvtnorm)

#Applying classification methods to 1st simulated data
#simulating dataset-1

d=5
mu1=rep(0,times=d)
mu2=rep(5,times=d)
simulated_matrix1=matrix(0,1000,d)
y=c()
set.seed(1234)

for(i in 1:1000)
{
  U=runif(1,0,1)
  if(U<0.65)
  {
    simulated_matrix1[i,]=mvrnorm(1,mu1,diag(d))
    y[i]=0
  }
  else
  {
    simulated_matrix1[i,]=mvrnorm(1,mu2,diag(d))
    y[i]=1
  }

}
plot(simulated_matrix1)

#making a dataframe

simulated_dataset1=data.frame(y,simulated_matrix1)
simulated_dataset1

simulated_dataset1$y=as.factor(simulated_dataset1$y)
class(simulated_dataset1$y)

#Applying Naive Bayes to the data 
set.seed(1234)
misclassification_naive=c()

for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset1),replace=T,prob=c(0.8,0.2))
  train_set_naive_sim1=simulated_dataset1[indexset==1,]
  test_set_naive_sim1=simulated_dataset1[indexset==2,]
  
  # Fitting Naive Bayes Model 
  classifier_cl = naiveBayes(y ~ ., data = train_set_naive_sim1)
  classifier_cl
  
  # Predicting on test data'
  y_pred <- predict(classifier_cl, newdata = test_set_naive_sim1)
  y_pred
  compare=table(test_set_naive_sim1$y,y_pred)
  #print(compare)
  misclassification_naive=c(misclassification_naive,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
}
print(misclassification_naive)
miss_cl_naive=mean(misclassification_naive)
miss_cl_naive

#Logistic regression 


#checking linearity
logistic=glm(y~.,data=simulated_dataset1,family= binomial(link="logit"))

summary(logistic)
prob=predict(logistic,type= "response")

log_odds = log(prob/(1-prob))
plot_X1 = ggplot(simulated_dataset1, aes(x = X1, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X1", y = "Log-Odds") +
  theme_minimal()

plot_X2 = ggplot(simulated_dataset1, aes(x = X2, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X2", y = "Log-Odds") +
  theme_minimal()

plot_X3 = ggplot(simulated_dataset1, aes(x = X3, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X3", y = "Log-Odds") +
  theme_minimal()

plot_X4 = ggplot(simulated_dataset1, aes(x = X4, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X4", y = "Log-Odds") +
  theme_minimal()

plot_X5 = ggplot(simulated_dataset1, aes(x = X5 , y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X5", y = "Log-Odds") +
  theme_minimal()

grid.arrange(plot_X1,plot_X2,plot_X3,plot_X4,plot_X5,ncol = 3, top = "Scatter plot of Log odds vs Independent variable")

#checking multicollinearity
vif_score=car::vif(logistic, type= "predictor")
vif_score

#checking misclassification
set.seed(1234)
misclassification_logistic=c()

for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset1),replace=T,prob=c(0.8,0.2))
  train_set=simulated_dataset1[indexset==1,]
  test_set=simulated_dataset1[indexset==2,]
  
  #Logistic regression classification  
  
  # Fitting Logistic Regression 
  logistic=glm(y~.,data=train_set,family= binomial(link="logit"))
  
  # Predicting on test data'
  logistic_pred <- predict(logistic, newdata = test_set, type = "response")
  logistic_pred
  round(logistic_pred,1)
  compare=table(test_set$y,logistic_pred)
  misclassification_logistic=c(misclassification_logistic,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_logistic
  
}
print(misclassification_logistic)

miss_cl_log=mean(misclassification_logistic)
miss_cl_log

#Random forest
set.seed(1234)
misclassification_random=c()

#train_test split
for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset1),replace=T,prob=c(0.8,0.2))
  train_set=simulated_dataset1[indexset==1,]
  test_set=simulated_dataset1[indexset==2,]
  train_set
  
  # Fitting Random forest Model 
  rand_fmodel=randomForest(y ~ ., data = train_set,importance = TRUE,ntree = 500)
  rand_fmodel
  # Predicting on test data'
  rf_pred <- predict(rand_fmodel, newdata = test_set)
  rf_pred
  compare=table(test_set$y,rf_pred)
  print(compare)
  misclassification_random=c(misclassification_random,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_random
}
miss_cl_rf=mean(misclassification_random)
miss_cl_rf



#Performing on the 2nd simulated dataset
#simulating dataset-2
d=5
mu21=rep(0,times=d)
mu22=rep(1,times=d)
simulated_matrix2=matrix(0,1000,d)
y1=c()
set.seed(1234)

for(i in 1:1000)
{
  U=runif(1,0,1)
  if(U<0.65)
  {
    simulated_matrix2[i,]=mvrnorm(1,mu21,diag(d))
    y1[i]=0
  }
  else
  {
    simulated_matrix2[i,]=mvrnorm(1,mu22,diag(d))
    y1[i]=1
  }
  
}
plot(simulated_matrix2)

#making a dataframe

simulated_dataset2=data.frame(y1,simulated_matrix2)
simulated_dataset2

simulated_dataset2$y1=as.factor(simulated_dataset2$y1)
class(simulated_dataset2$y1)

#Applying Naive Bayes to the data
set.seed(1234)
misclassification_naive=c()

for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset2),replace=T,prob=c(0.8,0.2))
  train_set_naive_sim2=simulated_dataset2[indexset==1,]
  test_set_naive_sim2=simulated_dataset2[indexset==2,]
  
  # Fitting Naive Bayes Model 
  classifier_cl = naiveBayes(y1 ~ ., data = train_set_naive_sim2)
  classifier_cl
  
  # Predicting on test data'
  y_pred <- predict(classifier_cl, newdata = test_set_naive_sim2)
  y_pred
  compare=table(test_set_naive_sim2$y,y_pred)
  #print(compare)
  misclassification_naive=c(misclassification_naive,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
}
print(misclassification_naive)
miss_cl_naive=mean(misclassification_naive)
miss_cl_naive

#Logistic regression 

#linearity check
#checking linearity

logistic=glm(y1~.,data=simulated_dataset2,family= binomial(link="logit"))

summary(logistic)
prob=predict(logistic,type= "response")

log_odds = log(prob/(1-prob))
plot_X1 = ggplot(simulated_dataset2, aes(x = X1, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs( x = "X1", y = "Log-Odds") +
  theme_minimal()

plot_X2 = ggplot(simulated_dataset2, aes(x = X2, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X2", y = "Log-Odds") +
  theme_minimal()

plot_X3 = ggplot(simulated_dataset2, aes(x = X3, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X3", y = "Log-Odds") +
  theme_minimal()

plot_X4 = ggplot(simulated_dataset2, aes(x = X4, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X4", y = "Log-Odds") +
  theme_minimal()

plot_X5 = ggplot(simulated_dataset2, aes(x = X5 , y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "X5", y = "Log-Odds") +
  theme_minimal()

grid.arrange(plot_X1,plot_X2,plot_X3,plot_X4,plot_X5,ncol = 3, top = "Scatter plot of Log odds vs Independent variable")

#checking multicollinearity
vif_score=car::vif(logistic, type= "predictor")
vif_score
#checking misclassification
set.seed(1234)
misclassification_logistic=c()

for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset2),replace=T,prob=c(0.8,0.2))
  train_set=simulated_dataset2[indexset==1,]
  test_set=simulated_dataset2[indexset==2,]
  
  #Logistic regression classification  
  
  # Fitting Logistic Regression 
  logistic=glm(y1~.,data=train_set,family= binomial(link="logit"))
  
  # Predicting on test data'
  logistic_pred <- predict(logistic, newdata = test_set, type = "response")
  logistic_pred
  round(logistic_pred,1)
  compare=table(test_set$y,logistic_pred)
  misclassification_logistic=c(misclassification_logistic,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_logistic
  
}
print(misclassification_logistic)

miss_cl_log=mean(misclassification_logistic)
miss_cl_log

#Random forest

set.seed(1234)
misclassification_random=c()

#train_test split
for(i in 1:100){
  indexset=sample(1:2,nrow(simulated_dataset2),replace=T,prob=c(0.8,0.2))
  train_set=simulated_dataset2[indexset==1,]
  test_set=simulated_dataset2[indexset==2,]
  train_set
  
  # Fitting Random forest Model 
  rand_fmodel=randomForest(y1 ~ ., data = train_set,importance = TRUE,ntree = 500)
  rand_fmodel
  # Predicting on test data'
  rf_pred <- predict(rand_fmodel, newdata = test_set)
  rf_pred
  compare=table(test_set$y,rf_pred)
  print(compare)
  misclassification_random=c(misclassification_random,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_random
}
miss_cl_rf=mean(misclassification_random)
miss_cl_rf


