# CSP571
# Lecture 12

# Let's revisit the Titanic data set
# Download the Titanic data set from Kaggle: https://www.kaggle.com/c/titanic/data
data <- read.csv('/Users/adam.mcelhinney/Downloads/Titanic/train.csv'
                 , header= TRUE
                 ,na.strings=c(""))

head(data)


# Recall the variable cleaning we need to do for this data set
# Best practice would be to code this up into a single function

# Cabin is missing too much, so we may decide to drop it. Also, we don't want to
# model with PassengerId
varsToDrop <- c('Cabin', 'PassengerId')

colsBefore <- ncol(data)
data <- data[,names(data)[!names(data) %in% varsToDrop]]
colsAfter <- ncol(data)
head(data)

# Always code defensively!
stopifnot(colsBefore == colsAfter + length(varsToDrop))


# For age, we'll replace missing with the mean
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# Let's take a look at how R encoded in the factor vars
?contrasts
contrasts(data$Sex)

contrasts(data$Embarked)

# Let's do mode missing value replacement for Embarked
EmbarkedMode <- names(sort(table(data$Embarked), decreasing = TRUE)[1])
data[is.na(data$Embarked), 'Embarked'] <- EmbarkedMode

targetVar <- 'Survived'

# We'll do stratified sampling to split our data into training and test sets
library('caret')
inTrain <- createDataPartition(y = data[,targetVar], list = FALSE, p = .8)
train <- data[inTrain,]
test <- data[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(data))

xVars <- c("Pclass", 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked')


createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)



# Let's fit a decision tree model
# Code pretty heavily adapted from: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

library(rpart)
?rpart
fit <- rpart(modelForm,
             data=train,
             method="class")

# Let's take a look at the tree
fit

# Let's inspect the splitting criteria in more detail
summary(fit)

# Default plotting tools are terrible
plot(fit)
text(fit)


# Best plotting library for decision trees I have found is
# in the rattle package, but it requires a bunch of dependencies
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)


Prediction <- predict(fit, test, type = "class")
Actual <- test$Survived

confusionMatrix(reference = Actual, data = Prediction)

# Let's try messing with the complexity parameters for the model
fit2 <- rpart(modelForm,
             data=train,
             method="class",
               control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit2)

# WHOA! What do we think is going on?
# Do we think this model will perform better or worse than
# the previous model?





Prediction2 <- predict(fit2, test, type = "class")
Actual <- test$Survived

confusionMatrix(reference = Actual, data = Prediction2)




# Let's try some cross validation to optimize the hyperparameters
# of our model. The caret library can really help with this.
library(caret)
?trainControl
trctrl <- trainControl(method = "repeatedcv"
                       , number = 10, repeats = 3
                       , classProbs = TRUE
                       , summaryFunction = twoClassSummary
                       )
set.seed(3875)

# Caret works best if you specify the X and Y explicitly
x = train[,xVars]
# We need to convert this to a factor, which is annoying
y = as.factor(train[, targetVar])
fit3CV<- train(x = x
               , y = y
               , method = "rpart",
               # This is telling caret to test 20 options of the
               # hyperparameters (tuning variables) for this
               # model. Supposedly caret should know for rpart
               # what variables to tune. In practice, bugs are
               # aplenty!
       tuneLength=20,
       metric="ROC",
       trControl = trctrl)
fit3CV
plot(fit3CV)


# Let's extract the model that caret developed at plot it
fit3 <- fit3CV$finalModel
fancyRpartPlot(fit3)


Prediction3 <- predict(fit3CV, test, type = "raw")
Actual <- test$Survived

confusionMatrix(reference = Actual, data = Prediction3)
confusionMatrix(reference = Actual, data = Prediction)

# We see our model improved a bit! We could tune other parameters
# for this model if we wanted. We shall save that for another day.


# Let's try a random forest model.
#install.packages('randomForest')
library(randomForest)

# Notice how this takes significantly longer than fitting a
# decision tree, but still pretty fast!
fit4 <- randomForest(x = x, y = y
                    , data=train,
                    importance=TRUE,
                    # fit 2000 decision trees!
                    ntree=2000)

fit4

varImpPlot(fit4)

# Again, let's test our model
Prediction4 <- predict(fit4, test, type = "response")


# Hmmm, this model isn't quite as good as our best previous model
confusionMatrix(reference = Actual, data = Prediction4)
confusionMatrix(reference = Actual, data = Prediction3)


# Let's try to use caret again to tune our RF algo
fit5CV<- train(x = x
               , y = y
              # Why is this called 'rf'? This is silly given
              # its actually wrapping the 'randomForest' package
               , method = "rf",
               tuneLength=20,
              )
fit5CV
plot(fit5CV)

Prediction5 <- predict(fit5CV, test, type = "raw")


# Still not better!
confusionMatrix(reference = Actual, data = Prediction5)
confusionMatrix(reference = Actual, data = Prediction3)



