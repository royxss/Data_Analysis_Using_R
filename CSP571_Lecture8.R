# CSP571
# Lecture 8

# Logistic regression examples
# Download the Titanic data set from Kaggle: https://www.kaggle.com/c/titanic/data
data <- read.csv('/Users/SROY/Downloads/Titanic/train.csv'
                 , header= TRUE
                 ,na.strings=c(""))

head(data)
# Let's look at the missing data counts
missCounts <- sapply(data,function(x) sum(is.na(x)))
missCounts


# We can use the "Missing Map" to look at the distribution of missing
# data over each row
library(Amelia)
missmap(data, main = "Missing values ")

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

model <- glm(modelForm,family=binomial(link='logit'),data=train)

summary(model)

anova(model, test="Chisq")

# Let's see the performance on the test set


fitted.results <- predict(model
                          ,newdata = test[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')
hist(fitted.results)

test[,'fitted.results'] <- fitted.results


# We output the probabilities, but we want to turn the probabilities into
# a classification of survived or not. .5 is a reasonable starting cutoff.
survived.pred <- ifelse(fitted.results > 0.5,1,0)

mean(surived.pred)
mean(train[,targetVar])

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test[,targetVar]
                             , dnn = c("Predicted Surival", 'Actual Survival')
                             )
confusion


# Let's look at the ROC curve
library(ROCR)
pr <- prediction(fitted.results, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Let's look at the precision recall curves
library('DMwR')
PRcurve(preds = fitted.results, trues = test$Survived)

# Let's take a look at the deviance
llcomponents <- function(y, predicted.y){
  return(y*log(predicted.y) + (1-y)*log(1-predicted.y))
}
y <- train[,targetVar]
predicted.y <- predict(model
                       ,newdata = train[,xVars]
                       # Specifying response means we want the probabilities
                       ,type='response')

deviance <- sign(as.numeric(y) - predicted.y)*sqrt(-2*llcomponents(as.numeric(y), predicted.y))

summary(deviance)

# Extract the AIC
aic<- 2 * length(model$coefficients) - 2*logLik(model)
aic
AIC(model)
# Note that logLikelihoods in R are only defined up to an additive constant
# Different implementations of calculating the loglikelihood and therefore AIC
# may give different results. Ensure you are using the SAME implementation to
# compare model results relatively
alligator = data.frame(
  lnLength = c(3.87, 3.61, 4.33, 3.43, 3.81, 3.83, 3.46, 3.76,
               3.50, 3.58, 4.19, 3.78, 3.71, 3.73, 3.78),
  lnWeight = c(4.87, 3.93, 6.46, 3.33, 4.38, 4.70, 3.50, 4.50,
               3.58, 3.64, 5.90, 4.43, 4.38, 4.42, 4.25)
)
alli.mod1 = lm(lnWeight ~ lnLength, data = alligator)
AIC(alli.mod1)
extractAIC(alli.mod1)
# NOT THE SAME RESULT


# TODO: Add F1 score


# Pseudo-R**2
mod <- glm(y~x, family="binomial")
nullmod <- glm(y~1, family="binomial")
1-logLik(mod)/logLik(nullmod)

# Let's start with a reasonably strong affect of X on P(Y=1)
set.seed(63126)
n <- 10000
x <- 1*(runif(n)<0.5)
pr <- (x==1)*0.7+(x==0)*0.3
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family="binomial")
nullmod <- glm(y~1, family="binomial")
1-logLik(mod)/logLik(nullmod)


# Let's try a REALLY strong affect of X on P(Y=1)
set.seed(63126)
n <- 10000
x <- 1*(runif(n)<0.5)
pr <- (x==1)*0.9+(x==0)*0.1
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family="binomial")
nullmod <- glm(y~1, family="binomial")
1-logLik(mod)/logLik(nullmod)

# Takeaway, pseudo-R**2 will be reasonably low even in cases where there is a strong effect
# Cannot directly compare with an R**2 in another type of problem
