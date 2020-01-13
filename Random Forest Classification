# Random Forest Classification

# Importing the dataset
setwd("D:Machine-Learning-with-R-datasets-master")
dataset = read.csv("social_Network_Ads.csv")
str(dataset)
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Random Forest Classification to the Training set

install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

#hyperparameter tuning

## Manually tuning the parameters


install.packages('mlbench')
install.packages("devtools")
install.packages('caret')
install.packages("Rcpp")
install.packages("dplyr")
install.packages("data.table")
install.packages("e1071")
install.packages("usethis")
library(data.table)
library(dplyr)
library(Rcpp)
library(randomForest)
library(mlbench)
library(caret)
library(lattice)
library(Matrix)
library(devtools)
library(e1071)

control <- trainControl(method="repeatedcv", number=10, repeats=2, search="grid")

seed <- 7
metric <- "Accuracy"
set.seed(seed)
rnorm(7)
mtry <- sqrt(ncol(dataset))
tunegrid <- expand.grid(.mtry=mtry)
metric <- "Accuracy"

# trying different number of trees (10,15,20,25)
modellist <- list()
for (ntree in c(10, 15, 20, 25)) {
  set.seed(seed)
  
  fit <- train(Purchased~., data=dataset, method="rf", metric=metric,
               tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


# parameter tuning node size in R
modellis <- list()
for (nodesize in c(1,2)) {
  set.seed(seed)
  fit <- train(Purchased~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, nodesize=nodesize)
  key <- toString(nodesize)
  modellis[[key]] <- fit
}
# compare results
results <- resamples(modellis)
results
summary(results)
dotplot(results)









