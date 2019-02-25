rm(list = ls())
install.packages('boot')
library('boot')

data <- c(0.3057, 0.7227, 1.1566, 2.8622, 1.3588, 0.5377, 0.4336, 0.3426, 3.5784, 2.7694)
n <- length(data)

theta.ML <- (4*n)/sum(data)
theta.ML #2.843393

boot.fn <- function(x, index)
  return(4*length(index)/sum(x[index]))

set.seed(1)
boot.stat <- boot(data, boot.fn, 1000)
boot.stat

#Question 3
install.packages('ISLR')
install.packages('MASS')
install.packages('class')
install.packages('caret')
library('ISLR')
library('MASS')
library('class')
library('caret')

data <- read.csv("C://Users/saich/Desktop/Georgia State University Documents/Spring 2019/Classes/Machine Learning/HW/HW2/HeartData.csv",1)
train <- data[1:200,]
test <- data[201:297,]

#a.)
glm.fits <- glm(num~., data = train, family=binomial )
summ <- summary(glm.fits)
summ$coefficients

glm.predic <- predict(glm.fits, test, type = "response")
BTR <- ifelse(glm.predic>0.5, 1,0)
mis.Classification.Error <- mean(BTR!=test$num)
accuracy <- 1- mis.Classification.Error
accuracy

lda.fit <- lda(num~., data = train)
lda.pred <- predict(lda.fit, test)
mean(lda.pred$class==test[,14])

qda.fit <- qda(num~., data = train)
qda.pred <- predict(qda.fit, test)
mean(qda.pred$class==test[,14])

set.seed(1)
features <- c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal')
knn.pred_1 <- knn(train[, features], test[, features], train[,'num'], k=1 )
mean(knn.pred_1==test[,14])

set.seed(1)
knn.pred_5 <- knn(train[, features], test[, features], train[,'num'], k=5 )
mean(knn.pred_5==test[,14])

set.seed(1)
knn.pred_10 <- knn(train[, features], test[, features], train[,'num'], k=10 )
mean(knn.pred_10==test[,14])

#b.)
train_categ <- train
test_categ <- test
train_categ$sex <- factor(train_categ$sex)
train_categ$cp <- factor(train_categ$cp)
train_categ$fbs <- factor(train_categ$fbs)
train_categ$slope <- factor(train_categ$slope)
train_categ$exang <- factor(train_categ$exang)
train_categ$ca <- factor(train_categ$ca)
train_categ$thal <- factor(train_categ$thal)

test_categ$sex <- factor(test_categ$sex)
test_categ$cp <- factor(test_categ$cp)
test_categ$fbs <- factor(test_categ$fbs)
test_categ$slope <- factor(test_categ$slope)
test_categ$exang <- factor(test_categ$exang)
test_categ$ca <- factor(test_categ$ca)
test_categ$thal <- factor(test_categ$thal)

glm.fits <- glm(num~., data = train_categ, family=binomial )
summ <- summary(glm.fits)
summ$coefficients

glm.predic <- predict(glm.fits, test_categ, type = "response")
BTR <- ifelse(glm.predic>0.5, 1,0)
mis.Classification.Error <- mean(BTR!=test_categ$num)
accuracy <- 1- mis.Classification.Error
accuracy

lda.fit <- lda(num~., data = train_categ)
lda.pred <- predict(lda.fit, test_categ)
mean(lda.pred$class==test_categ[,14])

qda.fit <- qda(num~., data = train_categ)
qda.pred <- predict(qda.fit, test_categ)
mean(qda.pred$class==test_categ[,14])

set.seed(1)
features <- c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal')
knn.pred_1 <- knn(train_categ[, features], test_categ[, features], train_categ[,'num'], k=1 )
mean(knn.pred_1==test[,14])

set.seed(1)
knn.pred_5 <- knn(train_categ[, features], test_categ[, features], train_categ[,'num'], k=5 )
mean(knn.pred_5==test_categ[,14])

set.seed(1)
knn.pred_10 <- knn(train_categ[, features], test_categ[, features], train_categ[,'num'], k=10 )
mean(knn.pred_10==test_categ[,14])

#c.)
control <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "glm", family="binomial", trControl=control)
glm.fits$results$Accuracy

control <- trainControl(method = "loocv", savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "glm", family="binomial", trControl=control)
glm.fits$results$Accuracy

#LDA
control <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "lda", family="binomial", trControl=control)
glm.fits$results$Accuracy

control <- trainControl(method = "loocv", savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "lda", family="binomial", trControl=control)
glm.fits$results$Accuracy

#QDA
control <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "qda", family="binomial", trControl=control)
glm.fits$results$Accuracy

control <- trainControl(method = "loocv", savePredictions = TRUE)
glm.fits <- train(factor(num)~., data = data, method = "qda", family="binomial", trControl=control)
glm.fits$results$Accuracy
