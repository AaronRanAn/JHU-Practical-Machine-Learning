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

treefit <- train(Class ~ ., data=training, method="rpart")
treefit$finalModel

plot(treefit$finalModel, uniform = T, main = "Classification Result")

text(treefit$finalModel, use.n = T, all = T, cex = .5)

# Q3

library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

treefit2 <- train(Area ~ ., data=olive, method="rpart")

predict(treefit2, newdata = newdata) # 2.783282

# Q4

library(ElemStatLearn)
data(SAheart)

set.seed(8484)

train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

logitfit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                  data=trainSA, method="glm", family="binomial")

logitfit

missClass = function(values,prediction){
                                        sum(((prediction > 0.5)*1) != values)/length(values)
                                        }

train_phat = predict(logitfit, trainSA)

test_phat = predict(logitfit, testSA)

missClass(trainSA$chd, train_phat) # 0.2727273

missClass(testSA$chd, test_phat) # 0.3116883

# Q5 

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

set.seed(33833)

# set the y as the response variable

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

RFfit <- train(y ~ ., data = vowel.train, method = "rf", prox = T)

RFfit$finalModel # look at that confusion matrix.... 

order(varImp(RFfit$finalModel), decreasing=T)
