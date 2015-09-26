# quiz 4

# Question 1, model ensembling

library(ElemStatLearn)
library(caret)

data(vowel.train)
data(vowel.test) 


vowel.train$y <- as.factor(vowel.train$y)

vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

# fit rf and gbm for the training set

rf_fit <- train(y ~ ., data=vowel.train, method="rf")
gbm_fit <- train(y ~ ., data=vowel.train, method="gbm")

rf_hat <- predict(rf_fit, vowel.test)
gbm_hat <- predict(gbm_fit, vowel.test)

# extract the accuracy on from both methods

confusionMatrix(rf_hat, vowel.test$y)$overall[1] # 0.6038961

confusionMatrix(gbm_hat, vowel.test$y)$overall[1] # 0.5281385

# combine two methods into 1

pred_combine <- data.frame(rf_hat, gbm_hat, y=vowel.test$y, agree=rf_hat == gbm_hat)

# last col is a indicator for the agreement with two 

agree_pct <- table(pred_combine$agree)[2] / length(pred_combine$agree)

agree_pct

accuracy <- sum(rf_hat[pred_combine$agree] == pred_combine$y[pred_combine$agree]) / sum(pred_combine$agree)

accuracy

# Question 2, model ensembling

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# fit the model 

rf_fit2 <- train(diagnosis ~ ., data=training, method="rf")
gbm_fit2 <- train(diagnosis ~ ., data=training, method="gbm")
lda_fit2 <- train(diagnosis ~ ., data=training, method="lda")

# validate on test set, get accuracy

rf_hat2 <- predict(rf_fit2, testing)

gbm_hat2 <- predict(gbm_fit2, testing)

lda_hat2 <- predict(lda_fit2, testing)

ac_rf <- confusionMatrix(rf_hat2, testing$diagnosis)$overall[1]
ac_gbm <- confusionMatrix(gbm_hat2, testing$diagnosis)$overall[1]
ac_lda <- confusionMatrix(lda_hat2, testing$diagnosis)$overall[1]

# Ensemble the models 

pred_data <- data.frame(rf_hat2, gbm_hat2, lda_hat2, diagnosis=testing$diagnosis) # create ensembling data

emb_fit <- train(diagnosis ~., data=pred_data, method="rf")

fit_hat <- predict(emb_fit, testing)

ac_emb <- confusionMatrix(fit_hat, testing$diagnosis)$overall[1]

ac_emb # 0.8536585

ac_rf; ac_gbm; ac_lda

# Question 3: Lasso Regularization

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

lasso_fit <- train(CompressiveStrength ~ ., data=training, method="lasso")

lasso_fit

plot.enet(lasso_fit$finalModel, xvar="penalty", use.color=T) # Cement

# Question 4 Time Series

library(lubridate)  # For year() function below
dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)



