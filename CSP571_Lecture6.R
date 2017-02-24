#### CSP571
#### Lecture 6 R code

# Recall the insurnace data set
custdata <- read.table('https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv',
                       header=TRUE,sep='\t')

head(custdata)
summary(custdata)

library(ggplot2)

# Let's look at the relationship between income and health insurance
ggplot(custdata, aes(x = income, y = as.numeric(health.ins))) + geom_point() + geom_jitter(height = .1)+ stat_smooth()+ xlim(0, 100000)

# Notice how we "jitter" the data to better display overlapping values
# We see that slicing income at roughly 12500, 40000, 50000 might make sense

breaks <- c(0, 12500, 40000, 50000, Inf)

custdata$incomeBinned <- cut(custdata$income, breaks = breaks, include.lowest = TRUE)
str(custdata$incomeBinned)
levels(custdata$incomeBinned)


# Let's look at income on its own
ggplot(custdata, aes(x = income)) + geom_density()

# We see that its highly skewed. Lets apply a log transformation
custdata$incomeLog <- log(custdata$income, base=10)

ggplot(custdata, aes(x = incomeLog)) + geom_density()

# Notice that we are losing values <1. Log is not defined for those values.
# We can use the signed log transform to deal with this.
signedLog <- function(x){
  ifelse(abs(x) <= 1, 0, sign(x) * log10(abs(x)))
}
signedLog(-1)
custdata$incomeSignedLog <- sapply(custdata$income, FUN = signedLog)

ggplot(custdata, aes(x = incomeSignedLog)) + geom_density()




# Sampling data

# One way to implement sampling is to generate a uniform random number from 0-1
# Then if each number is less than our desired percentage for that group, allocate
# it to that group.
trainPct <- .8
testPct <- 1 - trainPct

# Generate a some random numbers between 0 and 1
# Always set a seed for reproducibility!!
set.seed(34543)
randVect <- runif(n = nrow(custdata), min = 0, max = 1)
# Convert to data.frame so we can plot in ggplot2
randDf <- as.data.frame(randVect)
ggplot(randDf, aes(x = randVect)) + geom_histogram()
trainGroup <- randVect<trainPct
testGroup <- !trainGroup
custTrain <- custdata[trainGroup,]
custTest <- custdata[testGroup,]
# Always check key assumptions like below!!!
stopifnot(nrow(custTrain) + nrow(custTest) == nrow(custdata))
nrow(custTrain)
nrow(custTest)
nrow(custdata)
# Note that the perecentages will not be exact, but as N gets large, they
# will be very close
nrow(custTrain)/nrow(custdata)
nrow(custTest)/nrow(custdata)


# Better way to actually implement this
custSample <- sample(custdata$custid, size = trainPct * nrow(custdata), replace = FALSE)
custTrain <- custdata[custdata$custid %in% custSample, ]
custTest <- custdata[!custdata$custid %in% custSample, ]
# Always check key assumptions like below!!!
stopifnot(nrow(custTrain) + nrow(custTest) == nrow(custdata))
nrow(custTrain)
nrow(custTest)
nrow(custdata)

# How different is the train and test data, with respect to health.ins
sum(custTrain$health.ins)/nrow(custTrain)
sum(custTest$health.ins)/nrow(custTest)


# Implement stratified sampling
library('caret')
inTrain <- createDataPartition(y = custdata$health.ins, p = trainPct, list = FALSE)
custTrain2 <- custdata[inTrain,]
custTest2 <- custdata[-inTrain,]
stopifnot(nrow(custTrain2) + nrow(custTest2) == nrow(custdata))

# How different is the train and test data, with respect to health.ins
# using stratified sampling? Should be much less.
sum(custTrain2$health.ins)/nrow(custTrain2)
sum(custTest2$health.ins)/nrow(custTest2)


# Dealing with categorical variables
