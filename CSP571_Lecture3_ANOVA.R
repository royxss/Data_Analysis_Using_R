lowCal <- c(8,  9,	6,	7,	3)
lowFat <- c(2,  4,	3,	5,	1)
lowCarb <- c(3,  5,	4,	2,	3)
control <- c(2,  2,	-1,	0,	3)

alpha = .05

# Compute the group means
meanLowCal <- mean(lowCal)
meanLowFat <- mean(lowFat)
meanLowCarb <- mean(lowCarb)
meanControl <- mean(control)

allObs <- c(lowCal, lowFat, lowCarb, control)
grandMean <- mean(allObs)


nLowCal <- length(lowCal)
nLowFat <- length(lowFat)
nLowCarb <- length(lowCarb)
nControl <- length(control)
N = length(allObs)


# Compute the Sum of Squares Between Groups (SSB)

SSB <-
  (nLowCal * (meanLowCal - grandMean)^2
  + nLowFat * (meanLowFat - grandMean)^2
  + nLowCarb * (meanLowCarb - grandMean)^2
  + nControl * (meanControl - grandMean)^2)

# Compute the Sum of Square Errors
lowCalSqDiff <- sum((lowCal - meanLowCal)^2)
lowFatSqDiff <- sum((lowFat - meanLowFat)^2)
lowCarbSqDiff <- sum((lowCarb - meanLowCarb)^2)
controlSqDiff <- sum((control - meanControl)^2)
SSE = lowCalSqDiff + lowFatSqDiff + lowCarbSqDiff + controlSqDiff

# Compute the degrees of freedom
df1 = 4 - 1
df2 = N - 4

# Compute the Means Squares
MSB = SSB / df1
MSE = SSE / df2

# Compute the F-statistic
F = MSB / MSE

# Find the critical value of F
# Reject H0 if F >= criticalValue
criticalValue = qf(p = 1 - alpha
   , df1 = df1
   , df2 = df2
   )

ifelse(F >= criticalValue, "Reject H0", "Fail to reject H0")

# Do Anova with R's built-in function
# We need to change the data
treatment <- c(
   rep('lowCal', 5)
  , rep('lowFat', 5)
  , rep('lowCarb', 5)
  , rep('control', 5)
  )
weightLoss <- c(lowCal, lowFat, lowCarb
                , control)

df = data.frame(treatment, weightLoss)

fit <- aov(weightLoss ~ treatment)
fit
summary(fit)
