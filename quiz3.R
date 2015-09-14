# quiz 3

# Q1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain <- segmentationOriginal$Case == "Train"

training = segmentationOriginal[inTrain, ]
testing = segmentationOriginal[-inTrain, ]

dim(training); dim(testing)

set.seed(125)

treefit <- train(Class ~ ., methods = "rpart", data = training)
print(treefit$finalModel)

# Q2

library(pgmm)
data(olive)
olive = olive[,-1]