library(dplyr)
library(ggplot2)
#Load csv
movies <- read.table("u.item", header = FALSE, sep = "|", quote = "\"")

#Check the structure. NOTE headers will be missing
str(movies)

#Add column names
colnames(movies) <- c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action", "Adventure","Animation","Childrens","Comedy","Crime","Documenatry","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

#Check the structure. NOTE headers will be present now
str(movies)

# Lets take out columns IS, ReleaseDate, VideoReleaseDate & IMDB
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

#Check if these columns have been removed
str(movies)

# Ensure only unique rows are present
movies = unique(movies)
# 18 observations are taken out
str(movies)

#Lets do historical clustering 

#Lets compute distance using genres columns using euclidean
distances =  dist(movies[2:20], method = "euclidean")

#Lets generate the cluster Dendogram
clusterMovies = hclust( distances, method = "ward")

#View the Dendogram
plot(clusterMovies)

#Lets create 10 clusters
clusterGroups = cutree(clusterMovies, k = 10)

#lets compute the probability of Action genre in each of 10 clusters
tapply(movies$Action,clusterGroups, mean)

#Create a matrix of cluster and probabilities of each genre in each cluster
probabilityGenresByCluster <- cbind(
  tapply(movies$Action, clusterGroups, mean),
  tapply(movies$Adventure, clusterGroups, mean),
  tapply(movies$Animation, clusterGroups, mean),
  tapply(movies$Childrens, clusterGroups, mean),
  tapply(movies$Comedy, clusterGroups, mean),
  tapply(movies$Crime, clusterGroups, mean),
  tapply(movies$Documenatry, clusterGroups, mean),
  tapply(movies$Drama, clusterGroups, mean),
  tapply(movies$Fantasy, clusterGroups, mean),
  tapply(movies$Horror, clusterGroups, mean),
  tapply(movies$FilmNoir, clusterGroups, mean),
  tapply(movies$Musical, clusterGroups, mean),
  tapply(movies$Mystery, clusterGroups, mean),
  tapply(movies$Romance, clusterGroups, mean),
  tapply(movies$SciFi, clusterGroups, mean),
  tapply(movies$Thriller, clusterGroups, mean),
  tapply(movies$War, clusterGroups, mean),
  tapply(movies$Western, clusterGroups, mean)
)
# create a data frame
probabilityGenresByCluster <- data.frame(probabilityGenresByCluster)
# Provide column names
colnames(probabilityGenresByCluster) <- c("Action", "Adventure","Animation","Childrens","Comedy","Crime","Documenatry","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

#Function to fetch features of each clusters as genres involved
fetchClusterFeature <- function(probabilityGenresByCluster, cluster_no, probability_thresh) {
  cluster1 <- probabilityGenresByCluster[cluster_no, ]
  features <- c(cluster_no)
  for (cols in 1:18) {
    if (cluster1[cols] > probability_thresh) {
      features <-  append(features, names(cluster1[cols]))
    }
  }
  features
}
#fetch features of each cluster
fetchClusterFeature(probabilityGenresByCluster,"1", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"2", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"3", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"4", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"5", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"6", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"7", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"8", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"9", 0.30) 

fetchClusterFeature(probabilityGenresByCluster,"10", 0.30) 

#Locate the cluster in which a movie was classified as
#locate the row number
subset(movies,Title == "Men in Black (1997)")
clusterGroups[257]
