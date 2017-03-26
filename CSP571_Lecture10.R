# CSP571
# Lecture 10

# Naive Bayes
# Let's revisit the Titanic data set
# Download the Titanic data set from Kaggle: https://www.kaggle.com/c/titanic/data
data <- read.csv('/Users/SROY/Downloads/Titanic/train.csv'
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

# Load in the proper functions for Naive Bayes
library(e1071)
model <- naiveBayes(modelForm, data = train)
model
class(model)
summary(model)

# Test the model on the test data
probs <- predict(model, newdata = test, type = 'raw')
# This SHOULD work. It does not. This is an example of the issues with R that
# at times you may encounter. Nope problem though. We'll roll our own solution.
preds <- predict(model, newdata = test, type = 'class')

preds <- (probs[,'0'] <= probs[,'1'])*1
summary(preds)
summary(train[,"Survived"])
conf_matrix <- table(preds, test$Survived)
confusionMatrix(conf_matrix)


