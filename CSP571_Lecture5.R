custdata <- read.table('https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv',
                       header=TRUE,sep='\t')

# R provides a summary function to quickly assess data
summary(custdata)


# Always examine the data types to ensure they are imported properly
# Many ways to do this
str(custdata)
sapply(custdata, class)

# Can be useful to get all the numeric data fields and categorical data
# fields as variables

numVars <- names(which(sapply(custdata, is.numeric)))
catVars <- names(which(sapply(custdata, is.factor)))
logicalVars <- names(which(sapply(custdata, is.logical)))

# Make sure you didn't miss any variables
stopifnot(length(numVars) + length(catVars) + length(logicalVars)== ncol(custdata))

# Looking for missing data
summary(custdata)
# Calculate percent NA
Null_Counter <- apply(custdata, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "0"))/length(x))
Null_Counter

# Why is is.employed nearly 33% missing?
# Need to investigate
table(custdata$is.employed, useNA = 'always')
boxplot(custdata$health.ins~custdata$is.employed)
table(custdata$health.ins, custdata$is.employed
      , useNA = 'always', dnn = c('health insurance', 'is employed'))


# Invalid Values
# Why is there negative income?
# Does this mean debt level or something else? Definitely need to investigate.
hist(custdata$income)
# Boxplot indicates many possible outliers on the high-end
boxplot(custdata$income)

# Why are there customers with zero age? Does this make sense?
summary(custdata$age)
table(custdata$age)
hist(custdata$age)
# Do we really have 18 customers over 90? 8 over 100?
sum(custdata$age >= 90)
sum(custdata$age >= 100)

# Data Range
# Let's revisit customer age
boxplot(custdata$age)
hist(custdata$age)
# If we are interesting in predicting health insurance for the whole
# population, is there sufficient variation?

# What about data with huge ranges?
hist(custdata$income, breaks = 100)
boxplot(custdata$income)
summary(custdata$income)
# We have some really rich people. Is this representative?


# Duplicate entries
# custid sounds like it should be a unique id. Is it?
sum(table(custdata$custid)>1)
# Are there other duplicate rows?
t <- duplicated(custdata)
sum(t)

# Data that changes value over time
# Let's look at the moving average
library(zoo)
plot(rollmean(custdata$age, k = 50), type = 'l')
# Is the mean decreasing? Note that we are assuming that the new observations
# were appended as additional rows and that previous row values
# were not changed. You need to verify these things in real life.
plot(rollmedian(custdata$age, k = 51), type = 'l')

# Let's look at the occurances of missing values over time
plot(is.na(custdata$is.employed))
# No immediate pattern observed


# Linear dependencies
custdata$ageMonths <- custdata$age * 12
numVars <- c(numVars, 'ageMonths')
cor(custdata$ageMonths, custdata$age)
# In real life, may not have a perfect correlation due to sublte rounding
# differences
library(caret)
# Function cannot take NA
complete <- custdata[complete.cases(custdata), ]
lincomb = caret::findLinearCombos(complete[, numVars])
colnames(custdata[, numVars])[lincomb$linearCombos[[1]]]

# Dealing with duplicate factors
custdata$stateCleaned <- trimws(custdata$state.of.res)
sum(custdata$stateCleaned != custdata$state.of.res)
custdata$stateCleaned <- tolower(custdata$state.of.res)
sum(custdata$stateCleaned != custdata$state.of.res)

# Identifying possible misspellings is hard
# One quick way is to use a string distance function, like Levenshtein
# distance, which measures the minimum number of single-character
# edits (i.e. insertions, deletions or substitutions) required to
# change one word into the other
library(stringdist)
a = names(table(custdata$state.of.res))
b = a
# Change this to illustrate how we can detect possible misspellings
b[1] = "Alaabma"
stringDist <- stringdistmatrix(a = a, b = b, method = 'lv', useNames = 'strings')
# Let's sort to only look at the lowest scores
library(reshape)
stringDist2 <- melt(stringDist)
t <- stringDist2[order(stringDist2$value, decreasing = FALSE),]
t
# Let's just look at the top 10
t[1:10,]
# We want to ignore those that are zero
t <- t[t$value >0,]
t[1:10,]

# Plotting and ggplot2
library(ggplot2)
library(scales)


# Comparison of R base graphics versus ggplot2
# Adapted from: http://www.noamross.net/blog/2012/10/5/ggplot-introduction.html
data.df <- data.frame(Plant = c("Plant1", "Plant1", "Plant1", "Plant2", "Plant2",  "Plant2")
    , Type = c(1, 2, 3, 1, 2, 3)
    , Axis1 = c(0.2, -0.4, 0.8, -0.2, -0.7, 0.1)
    , Axis2 = c(0.5, 0.3, -0.1, -0.3, -0.1, -0.8))


# Goal: plot some two-variable data, changing color and shape
# by the sub-category of data

# Let's try with base R
color_foo <- colorRampPalette(c("lightblue", "darkblue"))
colors <- color_foo(3)

plot(range(data.df[, 3]), range(data.df[, 4]), xlab = "Axis 1", ylab = "Axis 2",
     type = "n")
points(data.df$Axis1, data.df$Axis2, pch = c(1, 2)[data.df$Plant], col = colors[data.df$Type])
legend("topright", legend = c("Plant1", "Plant2"), pch = 1:2)
legend("bottomright", legend = c("Type1", "Type2", "Type3"), pch = 20, col = colors)

# Lots of code. Looks pretty bad.
# Lets try with ggplot2
p <- ggplot(data.df
        # These are the "aesthetics", or visual elements of the graph
       , aes(x = Axis1, y = Axis2, shape = Plant, color = Type))+ geom_point(size = 5)
 # geoms: describe type of geometric objects that represent data

# Note that sometimes using RStudio, you have to explicitly call
# print to view the ggplot2 graphs
print(p)

# Wow. This was a lot easier and looks a lot better.
# Let's look at some more of ggplot2 functionality.

ggplot(custdata) + # ggplot2 uses the ggplot() function to declare the graph object
 # The plus operator adds elements to the graph
geom_histogram(aes(x=age), binwidth=5, fill="gray") # x and y variables are called "aesthetics"
                              # which are specified with "aes"
# Compare the distribution with the summary statistics
summary(custdata$age)


# Let's take a look at the income variable
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)

# Notice that due to the skew, maybe it makes sense to rescale this variable
# Can rescale to a log base 10 axis
# Notice how ggplot2 makes this really easy
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +
  annotation_logticks(sides="bt")

# Lets look at a bar chart of the distribution of states people live
# Notice what happens if you don't flip the axis
ggplot(custdata) +
  geom_bar(aes(x = state.of.res), fill = 'blue')
# With flipped axis
ggplot(custdata) +
  geom_bar(aes(x = state.of.res), fill = 'blue') +
  coord_flip()


# The default sorting is alphabetical. This may be useful. Or at times
# you may want to sort by the quantity ascending or descening.
state <- as.data.frame(sort(table(custdata[,'state.of.res']), decreasing = TRUE))
state
names(state) <- c('state', 'count')
ggplot(state) + geom_bar(aes(x = state, y = count)
                         # This tells ggplot2 we dont want it to compute
                         # any statistics, we want it to plot as is
                         # Note that even still, it reorders the data.frame
                         # We can formally reorder it if we want
                         , stat = 'identity'
                         , fill = 'blue') +
                          coord_flip()


# Scatterplots
# What is the relationship betwen age and income?
ggplot(custdata, aes(x = age, y = income)) + geom_point()

# Notice how there is a few outliers that are making our graph not very
# approachable. Let's set a ylimit and an xlimit
ggplot(custdata, aes(x = age, y = income)) + geom_point() + ylim(0, 200000)+ xlim(18, 90)

# Relationship is very hard to see. Luckily we can fit lines to smooth the data
ggplot(custdata, aes(x = age, y = income)) + geom_point() + ylim(0, 200000)+ xlim(18, 90) + geom_smooth
# Note by default, geo_smooth uses a regression technique called
# "loess". We can manaully specify anything we want.
ggplot(custdata, aes(x = age, y = income)) + geom_point() + ylim(0, 200000)+ xlim(18, 90) + stat_smooth(method = 'lm')

ggplot(custdata, aes(x = age, y = income)) + geom_point() + ylim(0, 200000)+ xlim(18, 90) +  stat_smooth(formula = y ~ log(x))


# Bar charts with ggplot2
# Bar charts are useful for cases where we want to compare categorical
# variables
# What is the relationship between martial status and health insurance
# converage?
# Let's just look at marital status first
ggplot(custdata) + geom_bar(aes(x=marital.stat))

ggplot(custdata) + geom_bar(aes(x=marital.stat, fill = health.ins)
                            , position = 'stack') # Each category on top of the other
# Preferred method!
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill = health.ins)
                            , position = 'dodge') # Side by side is called "dodge". No idea why

# Recall that ggplot has the concept of "facets", which are multiple plots
# for each value of a categorical variable. VERY powerful to quickly
# investigate complex relationships
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill = health.ins)
                            , position = 'dodge')+ facet_wrap(~housing.type)

# Notice how the above was hard to read due to small values for some of the
# categories. We want different axis for each value of y. Fortunately
# ggplot lets us do this.
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill = health.ins)
                            , position = 'dodge')+ facet_wrap(~housing.type, scales = 'free_y')
# What are some times when you want the y-variales to have the same scale
# and some times when you want them to have different scales?