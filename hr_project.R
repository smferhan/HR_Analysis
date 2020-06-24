##### Estimating through Decison Tree method ######
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
getwd()
hr_train <- read.csv("hr_train.csv")
str(hr_train)
hr_train %>%
  is.na() %>%
  colSums()
hr_train$left <- as.factor(hr_train$left)
class(hr_train$left)
unique(hr_train$left)
model <- rpart(left ~ ., data = hr_train, method = "class")
print(model)
fancyRpartPlot(model)
plotcp(model)
print(model$cptable)
x <- which.min(model$cptable[,"xerror"])
new_cp <- model$cptable[x, "CP"]
new_cp
pruned_model <- prune(model, cp = 0.01)
fancyRpartPlot(pruned_model)
hr_test <- read.csv("hr_test.csv")
hr_test %>%
  is.na() %>%
  colSums()
str(hr_test)
pred <- predict(pruned_model, hr_test, type = "class")
table(pred)
paste("Result:-","1030 employees out of 4500 left the company")


#### Estimating through Random Forest method #####
library(ranger)
hr_train <- read.csv("hr_train.csv")
str(hr_train)
hr_train %>%
  is.na() %>%
  colSums()
hr_train$left <- as.factor(hr_train$left)
class(hr_train$left)
unique(hr_train$left)
model <- ranger(left ~ ., data = hr_train, num.trees = 500, respect.unordered.factors = "order")
model
pred <- predict(model, hr_test)$predictions
table(pred)
paste("Result:-","1080 employees out of 4500 left the company")



########### Estimation through GBM method ######
hr_train <- read.csv("hr_train.csv")
str(hr_train)
hr_test <- read.csv("hr_test.csv")
str(hr_test)
library(gbm)
library(Metrics)
set.seed(1)
model2 <- gbm(left ~ ., data = hr_train, distribution = "bernoulli", n.trees = 10000) 
summary(model2)
prediction <- predict(model2, hr_test, type = "response", n.trees = 10000)
class(prediction)



##### Estimation through Cross-Validation technique #########

library(caret)
hr_train <- read.csv("hr_train.csv")
str(hr_train)
hr_train %>%
  is.na() %>%
  colSums()
hr_train$left <- as.factor(hr_train$left)
class(hr_train$left)
unique(hr_train$left)
set.seed(42)
model <- train(left ~ .,
               tuneLength = 3,
               data = hr_train,
               method = "ranger",
               trControl = trainControl(
                 method = "cv",
                 number = 10,
                 verboseIter = TRUE
               )
               )
model
plot(model)
pred <- predict(model, hr_test)
table(pred)
paste("Result:-","1064 employees out of 4500 left the company")
