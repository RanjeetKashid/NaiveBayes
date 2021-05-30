########## Salary Data ##########

salary_train <- read.csv(file.choose())
salary_test  <- read.csv(file.choose())
salary <- rbind(salary_train,salary_test)


#### EDA ####

class(salary_train)
str(salary_train)
sum(is.na(salary_train))
sum(is.na(salary_test))
str(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)
str(salary_test)
salary_test$educationno <- as.factor(salary_test$educationno)
summary(salary_train)
summary(salary_test)

library(ggplot2)
ggplot(data = salary,aes(x=salary$Salary, y = salary$age, fill = salary$Salary))+geom_boxplot()+ggtitle("Box Plot")
plot(salary$workclass,salary$Salary)
levels(salary$workclass)
plot(salary$education,salary$Salary)
levels(salary$education)
plot(salary$maritalstatus,salary$Salary)
levels(salary$maritalstatus)
plot(salary$occupation,salary$Salary)
levels(salary$occupation)
plot(salary$relationship,salary$Salary)
levels(salary$relationship)
plot(salary$race,salary$Salary)
levels(salary$race)
plot(salary$sex,salary$Salary)
ggplot(data = salary,aes(x=salary$Salary, y=salary$capitalgain,fill = salary$Salary)) + geom_boxplot()+ggtitle("Box Plot")
ggplot(data = salary,aes(x=salary$Salary, y=salary$capitalloss,fill = salary$Salary)) + geom_boxplot()+ggtitle("Box Plot")
ggplot(data = salary,aes(x=salary$Salary, y=salary$hoursperweek,fill = salary$Salary)) + geom_boxplot()+ggtitle("Box Plot")
plot(salary$native,salary$Salary)
levels(salary$native)


#### Feature selection ####

## Cramer's V ##

# >.5       - high association
# .3 to .5  - moderate association
# .1 to .3  - low association
# 0 to .1   - little if any association

install.packages("rcompanion")
library(rcompanion)

cramerV(salary$maritalstatus,salary$Salary,bias.correct = TRUE) #0.4487
cramerV(salary$relationship,salary$Salary,bias.correct = TRUE) #0.4548
cramerV(salary$race,salary$Salary,bias.correct = TRUE) #0.09957
cramerV(salary$sex,salary$Salary,bias.correct = TRUE) #0.2157
cramerV(salary$native,salary$Salary,bias.correct = TRUE) #0.09599

## Uncertainty coeff ##

# U(X|Y) -> Given Y, what fraction of the biths of X can we predict

install.packages("DescTools")
library(DescTools)

# given marital status what bits of salary can we predict
UncertCoef(salary$Salary,salary$maritalstatus,direction = "column") # 0.0865
UncertCoef(salary$Salary,salary$relationship,direction = "column") # 0.0776
UncertCoef(salary$Salary,salary$race,direction = "column") # 0.0106
UncertCoef(salary$Salary,salary$sex,direction = "column") # 0.0407
UncertCoef(salary$Salary,salary$native,direction = "column") # 0.0108

# by cramers rule and uncertainty coeff we can see that race and native have very less correlation with salary, so we can drop them.


#### Defining i/p - o/p ####

# race = 8 ; native = 13

salary_train_new <- salary_train[,c(1:7,9:12,14)]
salary_test_new  <- salary_test[,c(1:7,9:12,14)]

train_x <- salary_train[,c(1:7,9:12)]
train_y <- salary_train[,14]

test_x <- salary_test[,c(1:7,9:12)]
test_y <- salary_test[,14]

# Training model

library(e1071) 


install.packages("klaR", dep = TRUE)
library(klaR)
install.packages("questionr", dep = TRUE)
library(questionr)

### Simple Model ###

salary_classifier <- naiveBayes(salary_train_new$Salary ~.,data = salary_train_new)
salary_classifier$levels
summary(salary_classifier)

# Evaluating performance
salary_pred <- predict(salary_classifier,newdata = test_x)

table1 <- table(salary_pred,test_y)
accuracy1 <- (sum(diag(table1))/sum(table1))
accuracy1 # 81.76 %

# Using laplace

salary_classifier_l <- naiveBayes(salary_train_new$Salary ~.,data = salary_train_new,laplace = 15)
salary_pred_l <- predict(salary_classifier_l,newdata = test_x)

table2 <- table(salary_pred_l,test_y)
accuracy2 <- (sum(diag(table2))/sum(table2))
accuracy2 # 81.73 %

### Class Imbalance and hyperparameter tuning ###

library(modeldata)

install.packages("klaR", dep = TRUE)
install.packages("questionr", dep = TRUE)


# model = train(train_x,train_y,'nb',trControl = trainControl(method = 'cv',number = 10)) # Not able to use because if issue in installing questionr package required for klaR

model <- naiveBayes(salary_train_new$Salary~.,data = salary_train_new,trControl = trainControl(method = 'cv',number = 10))
train_pred <- predict(model,newdata = train_x)
table3 <- table(train_pred,train_y)
accuracy3 <- (sum(diag(table3))/sum(table3))
accuracy3 # 82.19 % for 10

