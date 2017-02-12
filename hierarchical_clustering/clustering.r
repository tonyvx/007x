# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)
library(NbClust)
# Now load the data and look at the first few rows
data(wine, package = "rattle")
head(wine)
str(wine)

sum(is.na(wine))
# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])
head(df)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc,
       wss,
       type = "b",
       xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
  print(wss[1])
}
wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   3 Clusters
#   * Why does this method work? What's the intuition behind it?
#   Law of diminishing returns or "The elbow method" - One should choose a number of clusters so that adding another cluster doesn't give much better modeling of the data.
#   * Look at the code for wssplot() and figure out how it works
#   This is doing a plot of withinss by number of cluster, after 6 clusters we see we are not loosing much by not considering addional clusters

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab = "Numer of Clusters", ylab = "Number of Criteria",
		            main = "Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# 3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine$Type,fit.km$cluster)

# Since clusters are maostly matching the wine Type, clustering identified is fairly good

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(df,fit.km$cluster)

# clusplot is using 2 dimensional plot and is providing 55.41% picture of the clustering. 
# Maybe not enough to make a judgement ?
