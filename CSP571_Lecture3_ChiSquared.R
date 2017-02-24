library(MASS)
data(survey)

# Create the contigency table
tbl = table(survey$Smoke, survey$Exer)
tbl

n <- sum(tbl)
rows <- rowSums(tbl)
cols <- colSums(tbl)
r <- length(rows)
c <- length(cols)


library(reshape)
# Expand the table into one row for each combination
tbl2 <- melt(tbl)
# Preserve the naems
names(tbl2) <- c('Smoking', 'Exercise', 'Observed')
tbl2

# Compute the expected frequencies for each combination
# Note I wrote this for clarity, not efficiency
# There are better ways of doing this, but they make understanding the point
# more difficult.

# Create a place to stored the expected values
expectedVals <- c()
for(i in 1:nrow(tbl2)){
  # Look up the levels for smoking and exercise
  Smoking <- tbl2[i, 1]
  Exercise <- tbl2[i, 2]

  # Return the sums that we already computed
  # for that level of smoking and exercise
  SmokingVal <- rows[Smoking]
  ExerciseVal <- cols[Exercise]

  # Computed the expected count per the formula
  expected <- (SmokingVal * ExerciseVal)/n

  # Append the expected count to the vector
  expectedVals <- c(expectedVals, expected)
}


tbl2['Expected'] <- expectedVals
tbl2['(O - E)**2 / E'] <- (tbl2['Observed']- tbl2['Expected'])**2 / tbl2['Expected']
tbl2

chiSquaredVal = sum(tbl2['(O - E)**2 / E'])

# Compute the degrees of freedom as specified for this distribution
df = (r - 1) * (c - 1)

alpha = .05
criticalVal <- qchisq(p = 1 - alpha, df = df)
pVal <- pchisq(chiSquaredVal, df, lower.tail = FALSE)

if(chiSquaredVal >= criticalVal){
  print("We reject the null hypothesis")
} else {
  print("We fail to reject the null hypothesis")
}


# In practice, we use R's builtin method for this
chisq.test(tbl)
chisq.test(x = survey$Smoke, y = survey$Exer, simulate.p.value = TRUE
           , B = 1000000)

