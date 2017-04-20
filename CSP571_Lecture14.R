# CSP/MATH 571
# Lecture 14

library('data.table')

# Let's look at a statistic of crime in Chicago
# "https://data.cityofchicago.org/Public-Safety/Crimes-2015/vwwp-7yr9"

# Let's read in the data set using R's standard CSV reader
system.time(df.crimes <- read.csv("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Crimes_-_2015.csv", header=TRUE,sep=","))

# Let's try using data.table to read in the CSV
system.time(dt.crimes <- fread("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Crimes_-_2015.csv", header=TRUE,sep=","))

# WOW! 14x speed-up
# Note: I personally almost always use fread for reading in CSVs

# Note how data.tables are also data.frames
class(dt.crimes)

# Data table contains a number of special functions. One notable one is the
# .N function, which counts the number of items.

dt.crimes[,.N]

# The names function works similarly because its just calling the
# data.frame function
names(dt.crimes)


# Let's see how many calls there were per district. This type of
# grouping is where data table excels.
# Note that we are saying select all rows (no value of i), then
# group by the District column (note that this is not in quotes)
#, then use the .N attribute to count the results.
dt.crimes[,.N,by=District]

# We then want to order by the count descending
dt.crimes[,.N,by=District][order(-N)]

# This really shows the power of data.table. We can pass in arbitrary
# functions as inputs into the i or j arguments. For example,
# let's say we want to sample 500 rows, group by the FBI code and find
# the mean number of calls. This is a one-liner in data.table.
# Note that you could do this in multiple calls, but you would sacrifice
# some of the speed. Data table requires that these operations be done
# on one call in order to maximize the value of the speed-ups.
dt.crimes[sample(1:.N,500), mean(.N), by="FBI Code"]


# Notice how above data.table gives uselss variable names (V1) for the
# aggregations. We can assign the variable to a more meaningful name by
# passing in as a list.
# Also note the . operator, which is sorthand for list()
dt.crimes[sample(1:.N,500), .(mean=mean(.N)), by="FBI Code"]


# Let's say we want to perform multiple aggregations. This is done via a
# vector.
dt.crimes[,.(DateMin = min(Date), DateMax = max(Date), count = .N), by = c("Beat", "FBI Code")]


# Note that normally for the i or j arguments, quotes are not used
# However, if you want to select a column that has a space in the name
# or other special character, we wrap it in back-ticks as quotes.
dt.crimes[,`FBI Code`,]

# Let's say we want to normalize the FBI Codes by removing the letters.
# For example, we want to make 04A and 04B just be 04.
dt.crimes[,.N, by =strtrim(tolower(`FBI Code`), 2)]


# Note to create a new column, we need to use the := assignment opperator.
dt.crimes[,`FBI Code2` := `FBI Code`,]
names(dt.crimes)


# We can delete columns by setting them to NULL. This has the effect of
# telling R not to use an memory for this column.
dt.crimes[,`FBI Code2` := NULL, ]
names(dt.crimes)


# Let's say we only want to select NARCOTICS crimes, that did not ended in
# arrest. How could we do that?
dt.crimes[`Primary Type` == 'NARCOTICS' & `Arrest` == 'false', ,]



# Let's try doing some similar things in dplyr
library(dplyr)


# The filter operation allows us to pass in row selection criteria as arguments
df <- as.data.frame((dt.crimes))
# Note the use of the back-ticks again
filter(df, `Primary Type` == 'NARCOTICS')

# Can pass in many arguments
filter(df, `Primary Type` == 'NARCOTICS', `Arrest` == 'false')

# Slice operator let's us select by position
slice(df, 1:50)

# Arrange operator reorders items. By default it orders in ascending order.
arrange(df, Ward, `FBI Code`)

# We use the desc operation to change the ordering
arrange(df, desc(Ward), `FBI Code`)

# Select operator works like SQL-select. It allows us to determine which columns
# we want to extract.
select(df, Date, Block, Beat)

# The mutate function let's us create new columns
df <- mutate(df, heroin = grepl('heroin', Description, ignore.case = TRUE))
names(df)
table(df[,'heroin'])

slice(filter(df, heroin), 1:100)


# We can use the summarise function (note the British spelling) to perform
# aggregate functions
summarise(df, max(Date), min(Date), length(ID))


# group_by operation predictability allows us to group data account to a specified columns
# However, it doesn't actually return the grouped results. It returns a data structure
# that allows you to later quickly perform the aggregation.
group_by(df, `FBI Code`)
group_by(df, `FBI Code`, heroin)

# For example, let's say we wanted to know how many cases involved heroin for a given FBI Code
# and display only those cases.
# This is say, take the df and group it by FBI Code and heroin flag. Then pass the resulting object
# to the summarise function, where we count how many instances this has occured. Then pass the
# resulting object to the filter function and eliminate any cases that did NOT involve heroin.

heroinFBI <- group_by(df, `FBI Code`, heroin) %>% summarise(length(ID)) %>% filter(heroin)

# You can chain as many of these types of operations together as you want.
# If we wanted to do this all as a one-liner, it would look as below.
# Some people love this syntax and find it intuitive. Others hate it.
mutate(df, heroin = grepl('heroin', Description, ignore.case = TRUE)) %>% group_by(`FBI Code`, heroin) %>% summarise(length(ID)) %>% filter(heroin)

# So which is faster? Can depend on size of data and characteristics of data.
# Generally data.table is faster. Lets check for ourselves.
# Adapted from https://github.com/Rdatatable/data.table/wiki/Benchmarks-:-Grouping
require(data.table)
#N=2e9; K=100
# We will change this from the original test to run in more reasonable time
N=2e6; K=100
set.seed(1)
DT <- data.table(
  id1 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id2 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE),                          # large groups (int)
  id5 = sample(K, N, TRUE),                          # large groups (int)
  id6 = sample(N/K, N, TRUE),                        # small groups (int)
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(5, N, TRUE),                          # int in range [1,5]
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
)
cat("GB =", round(sum(gc()[,2])/1024, 3), "\n")
system.time( DT[, sum(v1), keyby=id1] )
system.time( DT[, sum(v1), keyby=id1] )
system.time( DT[, sum(v1), keyby="id1,id2"] )
system.time( DT[, sum(v1), keyby="id1,id2"] )
system.time( DT[, list(sum(v1),mean(v3)), keyby=id3] )
system.time( DT[, list(sum(v1),mean(v3)), keyby=id3] )
system.time( DT[, lapply(.SD, mean), keyby=id4, .SDcols=7:9] )
system.time( DT[, lapply(.SD, mean), keyby=id4, .SDcols=7:9] )
system.time( DT[, lapply(.SD, sum), keyby=id6, .SDcols=7:9] )
system.time( DT[, lapply(.SD, sum), keyby=id6, .SDcols=7:9] )


## dplyr run

require(dplyr)
#N=2e9; K=100
# We will change this from the original test to run in more reasonable time
N=2e6; K=100
set.seed(1)
DF <- data.frame(stringsAsFactors=FALSE,
                 id1 = sample(sprintf("id%03d",1:K), N, TRUE),
                 id2 = sample(sprintf("id%03d",1:K), N, TRUE),
                 id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE),
                 id4 = sample(K, N, TRUE),
                 id5 = sample(K, N, TRUE),
                 id6 = sample(N/K, N, TRUE),
                 v1 =  sample(5, N, TRUE),
                 v2 =  sample(5, N, TRUE),
                 v3 =  sample(round(runif(100,max=100),4), N, TRUE)
)
cat("GB =", round(sum(gc()[,2])/1024, 3), "\n")
system.time( DF %>% group_by(id1) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id1) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id1,id2) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id1,id2) %>% summarise(sum(v1)) )
system.time( DF %>% group_by(id3) %>% summarise(sum(v1),mean(v3)) )
system.time( DF %>% group_by(id3) %>% summarise(sum(v1),mean(v3)) )
system.time( DF %>% group_by(id4) %>% summarise_each(funs(mean), 7:9) )
system.time( DF %>% group_by(id4) %>% summarise_each(funs(mean), 7:9) )
system.time( DF %>% group_by(id6) %>% summarise_each(funs(sum), 7:9) )
system.time( DF %>% group_by(id6) %>% summarise_each(funs(sum), 7:9) )
