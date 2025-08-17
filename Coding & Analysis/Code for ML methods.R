install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("broom")
install.packages("car")
install.packages("gridExtra")
install.packages("GGally")

library(GGally)
library(ggplot2)
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

dataset1=read.csv(file.choose(),header=T)
#missing value check
sum(is.na(data))
dataset1$Status=as.factor(dataset1$Status)
class(dataset1$Status)#chceking if factor or not

#scatter plots between dependent and independent variables 

plot1 <- ggplot(dataset1, aes(x = Length, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Length vs Status") +
  theme_minimal()

plot2 <- ggplot(dataset1, aes(x = Left, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Left vs Status") +
  theme_minimal()

plot3 <- ggplot(dataset1, aes(x = Right, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Right vs Status") +
  theme_minimal()

plot4 <- ggplot(dataset1, aes(x = Bottom, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Bottom vs Status") +
  theme_minimal()

plot5 <- ggplot(dataset1, aes(x = Top, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Top vs Status") +
  theme_minimal()

plot6 <- ggplot(dataset1, aes(x = Diagonal, y = Status, color = Status)) + 
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  ggtitle("Diagonal vs Status") +
  theme_minimal()


plot1
plot2
plot3
plot4
plot5
plot6

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,ncol = 3, top = "Scatter plot of Status vs Independent variable")

#Naive Bayes

set.seed(1234)
misclassification_naive=c()

for(i in 1:100){
  indexset=sample(1:2,nrow(dataset1),replace=T,prob=c(0.8,0.2))
  train_set_naive=dataset1[indexset==1,]
  test_set_naive=dataset1[indexset==2,]
  
  # Fitting Naive Bayes Model 
  classifier_cl <- naiveBayes(Status ~ ., data = train_set_naive)
  classifier_cl
  
  # Predicting on test data'
  y_pred <- predict(classifier_cl, newdata = test_set_naive)
  y_pred
  compare=table(test_set_naive$Status,y_pred)
  print(compare)
  misclassification_naive=c(misclassification_naive,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,2]+compare[2,1]+compare[2,2]))
}
print(misclassification_naive)
miss_cl_naive=mean(misclassification_naive)
miss_cl_naive

#Logistic Regression

#checking outliers
b_plot_length=boxplot(dataset1$Length)
b_plot_left=boxplot(dataset1$Left)
b_plot_right=boxplot(dataset1$Right)
b_plot_bottom=boxplot(dataset1$Bottom)
b_plot_top=boxplot(dataset1$Top)
b_plot_diagonal=boxplot(dataset1$Diagonal)

#Linearity Assumption Check

#checking linearity
logistic=glm(Status~.,data=dataset1,family= binomial(link="logit"))

summary(logistic)
 prob=predict(logistic,type= "response")

log_odds = log(prob/(1-prob))
plot_length = ggplot(dataset1, aes(x = Length, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Length vs. Log-Odds", x = "Length", y = "Log-Odds") +
  theme_minimal()

plot_left = ggplot(dataset1, aes(x = Left, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Left vs. Log-Odds", x = "Left", y = "Log-Odds") +
  theme_minimal()

plot_right = ggplot(dataset1, aes(x = Right, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Right vs. Log-Odds", x = "Right", y = "Log-Odds") +
  theme_minimal()

plot_bottom = ggplot(dataset1, aes(x = Bottom, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Bottom vs. Log-Odds", x = "Bottom", y = "Log-Odds") +
  theme_minimal()

plot_top = ggplot(dataset1, aes(x = Top, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Top vs. Log-Odds", x = "Top", y = "Log-Odds") +
  theme_minimal()

plot_diagonal = ggplot(dataset1, aes(x = Diagonal, y = log_odds)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Diagonal vs. Log-Odds", x = "Diagonal", y = "Log-Odds") +
  theme_minimal()

grid.arrange(plot_length,plot_left,plot_right,plot_bottom,plot_top,plot_diagonal,ncol = 3, top = "Scatter plot of Log odds vs Independent variable")

#checking multicollinearity
vif_score=car::vif(logistic, type= "predictor")
vif_score #Bottom has the highest value
barplot(vif_score, main = "VIF Scores", horiz = TRUE, col = "steelblue",xlim = c(0,15),names.arg = c("X","Length","Left","Right","Bottom","Top","Diagona"))
abline(v = 5, col = "red", lty = 2)

#New model after dropping bottom 
logistic1=glm(Status ~ Top+Length+Left+Right+Top+Diagonal,data=dataset1,family= binomial(link="logit"))
vif_score=car::vif(logistic1, type= "predictor")
vif_score
#After checking the VIF we observe Bottom has the highest VIF ~12 ... hence dropping it and running the model above. That shows that only Diagonal is significant
summary(logistic1)

#hence rerunning the model with Diagonal as only independent variable

logistic2=glm(Status ~ Diagonal,data=dataset1,family= binomial(link="logit"))
summary(logistic2)
logistic_pred1 <- predict(logistic1, newdata = dataset1)
compare1=table(dataset1$Status,logistic_pred1) 
prob1=predict(logistic1,type= "response") 
log_odds1 =log(prob1/(1-prob1)) 

#plotting scatter plot of log odds after reduction 

plot_length = ggplot(dataset1, aes(x = Length, y =
log_odds1)) + geom_point(alpha = 0.5) + geom_smooth(method = "loess", se =
FALSE, color = "red") + labs(title = "Length vs. Log-Odds", x = "Length", y =
"Log-Odds") + theme_minimal()

plot_right = ggplot(dataset1, aes(x = Right, y = log_odds1)) + geom_point(alpha
= 0.5) + geom_smooth(method = "loess", se = FALSE, color = "red") + labs(title
= "Right vs. Log-Odds", x = "Right", y = "Log-Odds") + theme_minimal()

plot_left = ggplot(dataset1, aes(x = Left, y = log_odds1)) +
geom_point(alpha = 0.5) + geom_smooth(method = "loess", se = FALSE, color =
"red") + labs(title = "Bottom vs. Log-Odds", x = "Bottom", y = "Log-Odds") +
theme_minimal()

plot_top = ggplot(dataset1, aes(x = Top, y = log_odds1)) + geom_point(alpha =
0.5) + geom_smooth(method = "loess", se = FALSE, color = "red") + labs(title =
"Top vs. Log-Odds", x = "Top", y = "Log-Odds") + theme_minimal()

plot_diagonal = ggplot(dataset1, aes(x = Diagonal, y = log_odds1)) +
geom_point(alpha = 0.5) + geom_smooth(method = "loess", se = FALSE, color =
"red") + labs(title = "Diagonal vs. Log-Odds", x = "Diagonal", y = "Log-Odds")+ theme_minimal()

grid.arrange(plot_length,plot_right,plot_left,plot_top,plot_diagonal,ncol =
3, top = "Scatter plot of Log odds vs Independent variable")

set.seed(1234)
misclassification_logistic=c()
misclassification_logistic1=c()
#train_test split
for(i in 1:100){
  indexset=sample(1:2,nrow(dataset1),replace=T,prob=c(0.8,0.2))
  train_set=dataset1[indexset==1,]
  test_set=dataset1[indexset==2,]
  
  #Logistic regression classification  
  
  # Fitting Logistic Regression 
  logistic=glm(Status~.,data=train_set,family= binomial(link="logit"))
  logistic1=glm(Status~Diagonal,data=train_set,family= binomial(link="logit"))
  
  # Predicting on test data'
  logistic_pred <- predict(logistic, newdata = test_set, type = "response")
  logistic_pred
  round(logistic_pred,1)
  compare=table(test_set$Status,logistic_pred)
  misclassification_logistic=c(misclassification_logistic,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_logistic
  
  logistic_pred1 <- predict(logistic1, newdata = test_set,type = "response")
  logistic_pred1
  round(logistic_pred1,1)
  compare=table(test_set$Status,logistic_pred1)
  #print(compare)
  misclassification_logistic1=c(misclassification_logistic1,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,2]+compare[2,1]+compare[2,2]))
  misclassification_logistic1
}
print(misclassification_logistic)
print(misclassification_logistic1)
miss_cl_log=mean(misclassification_logistic)
miss_cl_log1=mean(misclassification_logistic1)

miss_cl_log

miss_cl_log1

#Random forest


set.seed(1234)
misclassification_random=c()

#train_test split
for(i in 1:100){
  indexset=sample(1:2,nrow(dataset1),replace=T,prob=c(0.8,0.2))
  train_set=dataset1[indexset==1,]
  test_set=dataset1[indexset==2,]
  train_set
  
  # Fitting Random forest Model 
  rand_fmodel=randomForest(Status ~ ., data = train_set,importance = TRUE,ntree = 500)
  rand_fmodel
  # Predicting on test data'
  rf_pred <- predict(rand_fmodel, newdata = test_set)
  rf_pred
  compare=table(test_set$Status,rf_pred)
  print(compare)
  misclassification_random=c(misclassification_random,(compare[1,2]+compare[2,1])/(compare[1,1]+compare[1,1]+compare[2,1]+compare[2,2]))
  misclassification_random
}
miss_cl_rf=mean(misclassification_random)
miss_cl_rf

#Comparing all trhe 4 models visually through their misclassification rates

misclass_means <- c(miss_cl_naive, miss_cl_log, miss_cl_log1, miss_cl_rf)

# Model names
model_names <- c("Naive Bayes", "Logistic", "Logistic (Reduced)", "Random Forest")

# Colors for each model
bar_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")  # Blue, Orange, Green, Red

# Create the barplot
bar_positions <- barplot(
  misclass_means,
  names.arg = model_names,
  col = bar_colors,
  ylim = c(0, 0.6),
  main = "Comparison of Misclassification Rates Across Models",
  ylab = "Mean Misclassification Rate",
  border = "white",
  cex.names = 0.9,
  las = 1
)

# Adding value labels on top of each bar
text(
  x = bar_positions,
  y = misclass_means + 0.02,
  labels = round(misclass_means, 4),
  cex = 0.9,
  col = "black"
)


