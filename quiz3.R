# quiz 3

# Q1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain = createDataPartition(y=segmentationOriginal$Class,
                              p=0.7,list = F)

training = segmentationOriginal[inTrain, ]
testing = segmentationOriginal[-inTrain, ]

dim(training); dim(testing)

set.seed(125)

treefit <- train(Class ~ ., methods = "rpart", data = training)
print(treefit$finalModel)

