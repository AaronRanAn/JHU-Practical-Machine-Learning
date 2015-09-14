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

cartModel <- train(Class ~ ., data=training, method="rpart")
cartModel$finalModel

plot(treefit$finalModel, uniform = T, main = "Tree Classification")

text(treefit$finalModel, use.n = T, all = T, cex = .8)


# Q2

library(pgmm)
data(olive)
olive = olive[,-1]