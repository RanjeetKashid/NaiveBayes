##### Salary Data #####

salary_train <- read.csv(file.choose())
salary_test  <- read.csv(file.choose())
class(salary_train)
str(salary_train)
sum(is.na(salary_train))
sum(is.na(salary_test))

# Discretizing data - Training data

salary_train_n <- names(which(sapply(salary_train[,-14], is.factor)))

install.packages("CatEncoders")
library(CatEncoders)

for (i in salary_train_n){
  encode <- LabelEncoder.fit(salary_train[,i])
  salary_train[,i] <- transform(encode,salary_train[,i])
}

str(salary_train)

# Discretizing data - Test data

salary_test_n <- names(which(sapply(salary_test[,-14], is.factor)))

for (i in salary_test_n){
  encode <- LabelEncoder.fit(salary_test[,i])
  salary_test[,i] <- transform(encode,salary_test[,i])
}

str(salary_test)


# Defining i/p

train_x <- salary_train[,1:13]
train_y <- salary_train[,14]

test_x <- salary_test[,1:13]
test_y <- salary_test[,14]

# Training model

library(e1071)
salary_classifier <- naiveBayes(salary_train$Salary ~.,data = train_x)
salary_classifier$levels

# Evaluating performance
salary_pred <- predict(salary_classifier,newdata = test_x)

table1 <- table(salary_pred,test_y)
accuracy1 <- (sum(diag(table1))/sum(table1))
accuracy1

# Using laplace

salary_classifier_l <- naiveBayes(salary_train$Salary ~.,data = train_x,laplace = 15)
salary_pred_l <- predict(salary_classifier_l,newdata = test_x)

table2 <- table(salary_pred_l,test_y)
