# CSP571_Lecture_2

# Adapted from Zumel companion code
uciCar <- read.table(    # Note: 1
  'http://www.win-vector.com/dfiles/car.data.csv', 	# Note: 2
  sep=',', 	# CAUTION when using commas as the delimiter on text data
  header=T 	# Note: 4
)

class(uciCar)

summary(uciCar)

dim(uciCar)

# Another example

d <- read.table(paste('http://archive.ics.uci.edu/ml/',
                      'machine-learning-databases/statlog/german/german.data'
                      ,sep=''),
                # Best practice is to set this to FALSE and explicitly define
                # your factor variables in the code
                stringsAsFactors=F
                ,header=F)
print(d[1:3,])



# What types of variables are there in this data set?
library(MASS)
head(survey)
# Data dictionary
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/survey.html
summary(MASS)
# Identify the discrete and continuous variables
# Categorize each variable accodring to their scale:
# categorical, ordinal, interval, ratio
# Are there any dichotomous variables?
# Are there are binary variables?
# Are there any primary keys?
# Construct one of each from the data set


# Computing confidence intervals in R
# Adapted from http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-mean-unknown-variance
height.response <- na.omit(survey$Height)


n <- length(height.response)
s <- sd(height.response)
SE <- s/sqrt(n)
alpha <- .05

# Need to figure out z/alpha/2
zVal <- qnorm(p = 1-alpha/2)

E <- zVal*SE
xbar = mean(height.response)
xbar+c(-E,E)

# Obtaining data from web sources
# Webscraping (very basic introduction)
install.packages('rvest') # Webscraping package
install.packages('stringr') # Package that makes manipulating strings (text) easier
install.packages('tidyr') # Package for making data manipulation easier

library(rvest)
library(stringr)
library(tidyr)

# Let's take a look at this webpage and how html tables are constructed
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)
# How html tables are constructed: http://www.w3schools.com/html/html_tables.asp

sb_table <- html_nodes(webpage, css = 'table')
str(sb_table)
t <- sb_table[[1]]

# We want to read this hmtl table and convert it into a data frame
sb <- html_table(sb_table)[[1]]
head(sb)

# Note that the X1... is garbage, so we want to remove that
# Also it just repeads "Super Bowl Winners..." so we also want to remove that
sb <- sb[-(1:2), ]
# Let's assign the right names
names(sb) <- c("number", "date", "site", "result")
head(sb)


# The dates are not formatted in an easy to use way. Let's convert them

sb$date <- as.Date(sb$date, "%B. %d, %Y")
head(sb)


# The Super Bowl number is listed as a Roman numeral. That's annoying.
# Let's replace that.
sb$number <- 1:nrow(sb)

# Let's split the result into the winners and losers
sb <- separate(sb, result, c('winner', 'loser')
               , sep=', ' # We want to split this where the comma is located
               , remove=TRUE)
head(sb)

# We want to extract the scores from the end of winner and loser
# We use a reguluar expression (denoted as the pattern variable)
# This says to extract one or more numbers before the end of a line
# Regular expressions are not just an R concept, they are widely used in all
# major programming languages.
pattern <- " \\d+$"
sb$winnerScore <- as.numeric(str_extract(sb$winner, pattern))
sb$loserScore <- as.numeric(str_extract(sb$loser, pattern))
sb$winner <- gsub(pattern, "", sb$winner)
sb$loser <- gsub(pattern, "", sb$loser)
head(sb)

