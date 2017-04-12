# CSP 571
# Lecture 11



height <- c(185,
            170,
            168,
            179,
            182,
            188,
            180,
            180,
            183,
            180,
            180,
            177,
            170,
            175)

weight <- c(72,
            56,
            60,
            68,
            72,
            77,
            71,
            70,
            84,
            88,
            67,
            76,
            61,
            70)


cluster <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
cluster <- cluster + 1
colors <- ifelse(cluster == 2, 'red', 'blue')


# Let's plot these values
plot(x = height, y= weight, col = colors, type = 'p', cex = 1.5, pch = 19)

data <- data.frame(height, weight, cluster)

# We will use k = 2, because we "know" there are two clusters
# This is not a realistic assumption, but useful to explain how the algorithm works
k = 2



# Note: we could assign centers randomly
# We could also assign centers based on the data points that have max Euclidean distance
# We will manually assign centers for illustrative puroposes
set.seed(323)
#center.heights <- runif(k, min = min(height), max = max(height))
#center.weights <- runif(k, min = min(weight), max = max(weight))
center.heights <- c(183, 170)
center.weights <- c(75, 63)
# Try picking different center points and see how it affects the algorithm
# center.heights <- c(185, 170)
# center.weights <- c(72, 56)

centers <- data.frame(center.heights, center.weights)

# Let's plot the centers
plot(x = height, y= weight, col = colors, type = 'p', cex = 1.5, pch = 19)
points(x = centers[,'center.heights'], y = centers[,'center.weights'], bg = 'black'
       , cex = 2, pch = 22)

# Let's compute the Euclidean distance
euclid <- function(x1, y1, x2, y2){
  return(sqrt((x1 - x2) ^2 + (y1 - y2) ^2))
}

n <- nrow(data)

# Compute the distance to each cluster center
for(i in 1:k){
  dist <- c()

  for(j in 1:n){
    d <- euclid(data[j, 1], data[j, 2], centers[i, 1], centers[i, 2])
    dist <- c(dist, d)
  }

  data[,paste0(i,"_cluster")] <- dist
}

data

cols <- ncol(data)
# Assign each observation to the closest cluster
clusterAssign <- c()
for(j in 1:n){
  clustDist <- data[j, (cols - k + 1):cols]
  closestClust <- which.min(clustDist)[[1]]
  clusterAssign <- c(clusterAssign, closestClust)
}
data[,'assignedCluster'] <- clusterAssign

data

# Let's look at the accuracy of the cluster so far
1-mean(abs(data[,'cluster'] - data[,'assignedCluster']))

# Define the function to update the cluster center
centers.orig <- centers
for(i in 1:k){
  centers[,i] <- aggregate(data[,i], by = list(data[,'assignedCluster']), FUN = mean)[,2]
}

centers.orig
centers

plot(x = height, y= weight, col = colors, type = 'p', cex = 1.5, pch = 19)
points(x = centers.orig[,'center.heights'], y = centers.orig[,'center.weights'], bg = 'black'
       , cex = 2, pch = 22)
points(x = centers[,'center.heights'], y = centers[,'center.weights'], bg = 'green'
       , cex = 2, pch = 22)

# Iteration 2

# Again, compute the distance to each cluster center
for(i in 1:k){
  dist <- c()

  for(j in 1:n){
    d <- euclid(data[j, 1], data[j, 2], centers[i, 1], centers[i, 2])
    dist <- c(dist, d)
  }

  data[,paste0(i,"_cluster")] <- dist
}

data

# Assign each observation to the closest cluster
clusterAssign <- c()
for(j in 1:n){
  clustDist <- data[j, (cols - k + 1):cols]
  closestClust <- which.min(clustDist)[[1]]
  clusterAssign <- c(clusterAssign, closestClust)
}
data[,'assignedCluster'] <- clusterAssign

data

# Let's look at the accuracy of the cluster so far
1-mean(abs(data[,'cluster'] - data[,'assignedCluster']))

# Update the centers
centers.round2 <- centers
for(i in 1:k){
  centers[,i] <- aggregate(data[,i], by = list(data[,'assignedCluster']), FUN = mean)[,2]
}

centers.round2
centers


plot(x = height, y= weight, col = colors, type = 'p', cex = 1.5, pch = 19)
points(x = centers.orig[,'center.heights'], y = centers.orig[,'center.weights'], bg = 'black'
       , cex = 2, pch = 22)
points(x = centers.round2 [,'center.heights'], y = centers.round2 [,'center.weights'], bg = 'green'
       , cex = 2, pch = 22)
points(x = centers [,'center.heights'], y = centers [,'center.weights'], bg = 'yellow'
       , cex = 2, pch = 22)



