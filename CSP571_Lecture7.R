# CSP571 Lecture 7


# Let's walk through a regression example
library(NMF)

# Lets generate random matrices for explanatory purposes
set.seed(34553)
X = cbind(rmatrix(10, y = 1, dist=rnorm), rmatrix(10, y=1))
Y = rmatrix(10, y = 1, dist=rnorm)

# Using the formula for linear regression, let's calculate our vector of Betas
library(pracma)
XPrimeX <-  (t(X) %*% X)
# Notice how these are not equal. This is due to numerical instability issues.
XInv1 <- XPrimeX ^(-1)
XInv2 <- inv(XPrimeX)

beta <- inv(t(X) %*% X) %*% t(X) %*% Y

# Let's compare this versus what R would implement
data <- as.data.frame(X)
data['Y'] <- Y

model <- lm(Y~.-1, data = data)
summary(model)
model
beta

# What if we want to add an intercept to our equation?

X2 <- cbind(rep(1, times = nrow(X)), X)
beta2 <- inv(t(X2) %*% X2) %*% t(X2) %*% Y

data <- as.data.frame(X)
data['Y'] <- Y
model2 <- lm(Y~V1 + V2 +1, data = data)
summary(model2)
model2
beta2

# Compute our Yhat
yHat <- X %*% beta
# Compute our sum of squared errors
SSE <- sum((Y - yHat)**2)
# Compute our sum of squared regression
SSR <- sum((yHat - mean(Y))**2)
# Compute our total sum of squares
SST <- sum((Y - mean(Y))**2)

# Recall that SSE + SSR == SST
SSE + SSR
SST

# Notice the R-Squared value
# This is the percentage of variance that our model explains
summary(model2)
RSquared <- SSR/SST


# Let's try a larger example
load("/Users/SROY/Downloads/psub.RData")
head(psub)
# Data set contains a single annoymized person or household
# Occupation, education, additional demographic variables versus personal
# income level (PINCP)

# Let's start by exploring how age (AGEP), gender (SEX), class of worker (COW)
# and level of education (SCHL) affect personal income (PINCP)
# Note: In practice, we should ALWAYS start with extensive EDA and data cleaning
# However, for demonstration purposes we are skipping that step today.
xVars <- c('AGEP', 'SEX', 'COW', 'SCHL')
targetVar <- 'PINCP'

# Let's divide into training and testing
library('caret')
inTrain <- createDataPartition(y = psub[,targetVar], list = FALSE)
train <- psub[inTrain,]
test <- psub[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(psub))

# Let's create a function that will automatically make an R formula for us to use
# for regression modeling. We are going to use this frequently, so best coding
# practice is to encapsalate the logic into a function.
createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = FALSE)
model <- lm(modelForm, data = train)
summary(model)

# Notice how R has automatically handled the factor variables for us
levels(train[,'SCHL'])
levels(train[,'COW'])
levels(train[,'SEX'])

# Let's look at the distribution of our Target var
library('ggplot2')
ggplot(data=psub, aes(psub[,targetVar])) + geom_histogram

# Looks like the data is skewed. Let's see if we can improve our model by
# doing a log transform
targetVar2 = 'log(PINCP)'
modelForm2 <- createModelFormula(targetVar = targetVar2, xVars = xVars, includeIntercept = FALSE)
model2 <- lm(modelForm2, data = train)
summary(model2)
summary(model)
# We see that the R squared as significantly increased. Why?

# Let's take a look at our regression assumptions.
# 1. Linear relationship.
summary(model2)
# Since R**2 is very high, we are comfortable that this assumption is true
# 2. All variables have a multivariate normal relationship
# Let's use a QQplot to verify this
plot(model2)
# Not great. May be something to revisit.
# 3. No multicorlinearity
numVars <- names(which(sapply(psub[,xVars], is.numeric)))
# Only one numeric variable, so we should be okay
cor(psub[,numVars])
# 4. No autocorrelation
plot(model2)
# 5. Homoscedasticity
plot(model2)


# We want to evaluate our model on our test data
# We can leverage the predict function for this
targetVarHat <- paste0(targetVar2, "_hat")
test[,targetVarHat] <- predict(model2, test)
head(test[,targetVarHat])

# Calculate R**2 on the test data
test[,targetVar2] <- log(test[,targetVar])
test[,'residual'] <- test[,targetVarHat] - test[,targetVar2]
SST <- sum((test[,targetVar2] - mean(test[,targetVar2]))^2)
SSR <- sum((test[,targetVar2] - test[,targetVarHat])^2)
SSR/SST # Notice .71 in test set, .99 in train set
# Means model is slightly overfit, but still not bad performance
