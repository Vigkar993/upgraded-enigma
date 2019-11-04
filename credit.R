setwd("D:Machine-Learning-with-R-datasets-master")
credit<- read.csv("credit.csv")
str(credit)
install.packages("e1071")
library(DataExplorer)
library(C50)
library(rpart)
library(rpart.plot)
library(lattice)
library(gmodels)
library(caret)
library(rattle)
library(e1071)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)
#Data Preparation
credit$default <- as.factor(credit$default)
levels(credit$default) <- c("yes", "no")
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)
#splitting the data into training, testing and validation set
args(sample)
sample <- sample(1:3, size = nrow(credit), prob = c(0.6,0.2,0.2),replace = TRUE)
credit_train <- credit[sample == 1, ] 
credit_test <- credit[sample == 2, ]
credit_valid <- credit[sample == 3, ]
dim(credit_train)
dim(credit_test)
dim(credit_valid)
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
prop.table(table(credit_valid$default))
#Training the model
credit_model <- C5.0(credit_train[-17],factor(credit_train$default))
credit_model
summary(credit_model)
nrow(credit)
#Evaluating the model performance
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Hyperparameter tuning

set.seed(123)
entropy_model <- train(default ~ . , credit_validation, method = "rpart",
                       parms = list(split = "information"),
                       trControl = trainControl("cv", number = 10),
                       tuneLength = 10)

entropy_model$results
entropy_model$bestTune


set.seed(123)
gini_model <- train(default ~ . , credit_validation, method = "rpart",
                    parms = list(split = "gini"),
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 10)

gini_model$results
gini_model$bestTune

#Final Model

credit_model<- train(default ~ . , credit_train, method = "rpart",
                     parms = list(split = "information"))
credit_pred<- predict(credit_model, credit_test)
CrossTable(credit_pred, credit_test$default,prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
confusionMatrix(credit_pred,credit_test$default)


