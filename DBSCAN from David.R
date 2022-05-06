#DBSCAN from David

library(tidyverse)
library(dbscan)

data(iris)
head(iris)

# drop species col and scale all measurements to have 0 mean and variance 1
iris_scaled <- iris %>% select(-Species) %>% scale()

minpoints<-ncol(iris_scaled)+1
k<-ncol(iris_scaled)

kNNdistplot(iris_scaled, k = k)
lines(x = rep(0.8, 150)) 

iris_dbscan <- dbscan(x = iris_scaled, minPts = minpoints, eps = 0.8, borderPoints = TRUE)
iris_dbscan

hullplot(iris_scaled, iris_dbscan, 
         cex = 2, pch = as.numeric(as.factor(iris$Species)))

legend("topright",                    # Add legend to plot
       legend = c("setosa", 
                  "versicolor", 
                  "virginica"),
       pch = c(1,2,3))

"Notice that we only got 2 clusters even though there are 3 species. 
Why is this? It is because the clusters formed by versicolor and virginica 
are touching, so the dbscan can’t distinguish them."

#Below, I make all pairwise plots of the 4 variables. 
#We can see that in all plots, the veriscolor and virginica clusters touch.

for(i in 1:3){
  for(j in (i+1):4){
    dev.new()
    plot(iris_scaled[,c(i,j)], col = iris_dbscan$cluster, pch = as.numeric(as.factor(iris$Species)))
  }
}


#########################################################################
##########                 Simulation Data                 ##############
#########################################################################
# Generate random correlation matrix and mean vector
library(mixtools)
library(randcorr)
set.seed(1234)
sig1=randcorr(5)
sig2=randcorr(5)

# Generate two random multivariate distributions
c1 <- rmvnorm(n=100, runif(5,-10,10), sig1)
c2 <- rmvnorm(n=100, runif(5,-10,10), sig2)


colnames(c1) <-colnames(c2) <- c("x1","x2", "x3", "x4", "x5")

# Generate cluster assignments and form dataframe
cluster.assignment<-rep(1:2, each=100)
df<-data.frame(cbind(rbind(c1,c2)), cluster.assignment)

# Check to make sure assignments are correct
head(df)
tail(df)

##############################
########  DBSCAN  ############
##############################

# Set parameters for DBScan
minpoints<-ncol(df)+1
k<-ncol(df)

# Look at graph to set epsilon=1.5
dev.new()
kNNdistplot(df, k = k)
lines(x = rep(1.5, 200)) 

# Run dbscan
df_dbscan <- dbscan(x = df, minPts = minpoints, eps = 1.5, borderPoints = TRUE)
df_dbscan

# Look at PC plot of dbscan groupings
dev.new()
hullplot(df, df_dbscan, 
         cex = 2, pch = cluster.assignment)

legend("topright",                    # Add legend to plot
       legend = c("Cluster 1", 
                  "Cluster 2"),
       pch = c(1,2))

# Look at pairwise groupings
for(i in 1:4){
  for(j in (i+1):5){
    dev.new()
    plot(df[,c(i,j)], col = df$cluster.assignment, pch = df$cluster.assignment)
  }
}

# Define and display accuracy of the DBScan regression
dbscan.table = table(df$cluster.assignment, df_dbscan$cluster)
accuracy.dbscan<-(dbscan.table[1,2]+dbscan.table[2,3])/200
accuracy.dbscan


#############################
########  K-Means  ##########
#############################
# Set up K-Means regression
# Load libraries
library(factoextra)
library(cluster)

# Visualize total within-group sums of squares by number of clusters
fviz_nbclust(df[,1:5], kmeans, method = "wss")

# Calculate gap statistic based on number of clusters
gap_stat <- clusGap(df[,1:5],
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

# Plot number of clusters vs. gap statistic; choose max as optimal number of clusters
fviz_gap_stat(gap_stat)

# Perform k-means clustering with k = 2 clusters
km <- kmeans(df[,1:5], centers = 2, nstart = 25)

# Plot results of final k-means model
fviz_cluster(km, data = df)

# Find means of each cluster
aggregate(df[,1:5], by=list(cluster=km$cluster), mean)

# Define and display accuracy
accuracy.kmeans=1-sum(df$cluster.assignment != km$cluster)
accuracy.kmeans


#############################
#####   Hierarchical  #######
#############################
library(factoextra)
library(cluster)

#perform hierarchical clustering 
clust.average <- agnes(df[,1:5], method = "average")
clust.single <- agnes(df[,1:5], method = "single")
clust.complete <- agnes(df[,1:5], method = "complete")

#produce dendrogram
pltree(clust.average, cex = 0.6, hang = -1, main = "Dendrogram") 
pltree(clust.single, cex = 0.6, hang = -1, main = "Dendrogram") 
pltree(clust.complete, cex = 0.6, hang = -1, main = "Dendrogram") 




#Messy Data
#########################################################################
##########                 Simulation Data                 ##############
#########################################################################
# Generate random correlation matrix and mean vector
library(randcorr)
library(mixtools)
set.seed(1234)
sig1=randcorr(5)
sig2=randcorr(5)

meanvector <- runif(5,-10,10)

# Generate two random multivariate distributions
c1 <- rmvnorm(n=100, meanvector, sig1)
c2 <- rmvnorm(n=100, meanvector+rnorm(5,0,1), sig2)


colnames(c1) <-colnames(c2) <- c("x1","x2", "x3", "x4", "x5")

# Generate cluster assignments and form dataframe
cluster.assignment<-rep(1:2, each=100)
df<-data.frame(cbind(rbind(c1,c2)), cluster.assignment)

# Check to make sure assignments are correct
head(df)
tail(df)

##############################
########  DBSCAN  ############
##############################

# Set parameters for DBScan
minpoints<-ncol(df[,1:5])+1
k<-ncol(df[,1:5])

# Look at graph to set epsilon=2
dev.new()
kNNdistplot(df, k = k)
lines(x = rep(1.5, 200)) 

# Run dbscan
df_dbscan <- dbscan(x = df[,1:5], minPts = minpoints, eps = 1.15, borderPoints = TRUE)
df_dbscan

# Look at PC plot of dbscan groupings
dev.new()
hullplot(df, df_dbscan, 
         cex = 2, pch = cluster.assignment)

legend("topright",                    # Add legend to plot
       legend = c("Cluster 1", 
                  "Cluster 2"),
       pch = c(1,2))

# Look at pairwise groupings
for(i in 1:4){
  for(j in (i+1):5){
    dev.new()
    plot(df[,c(i,j)], col = df$cluster.assignment, pch = df$cluster.assignment)
  }
}

# Define and display accuracy of the DBScan regression
dbscan.table = table(df$cluster.assignment, df_dbscan$cluster)
accuracy.dbscan<-(dbscan.table[1,2]+dbscan.table[2,3])/200
accuracy.dbscan


#############################
########  K-Means  ##########
#############################
# Set up K-Means regression
# Load libraries
library(factoextra)
library(cluster)

# Visualize total within-group sums of squares by number of clusters
fviz_nbclust(df[,1:5], kmeans, method = "wss")

# Calculate gap statistic based on number of clusters
gap_stat <- clusGap(df[,1:5],
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

# Plot number of clusters vs. gap statistic; choose max as optimal number of clusters
fviz_gap_stat(gap_stat)

# Perform k-means clustering with k = 2 clusters
km <- kmeans(df[,1:5], centers = 2, nstart = 25)

# Plot results of final k-means model
fviz_cluster(km, data = df[,1:5])

# Find means of each cluster
aggregate(df[,1:5], by=list(cluster=km$cluster), mean)

# Define and display accuracy
accuracy.kmeans=1-sum(df$cluster.assignment != km$cluster)/200
accuracy.kmeans


#############################
#####   Hierarchical  #######
#############################
library(factoextra)
library(cluster)

#perform hierarchical clustering 
clust.average <- agnes(df[,1:5], method = "average")
clust.single <- agnes(df[,1:5], method = "single")
clust.complete <- agnes(df[,1:5], method = "complete")

#produce dendrogram
pltree(clust.average, cex = 0.6, hang = -1, main = "Dendrogram") 
pltree(clust.single, cex = 0.6, hang = -1, main = "Dendrogram") 
pltree(clust.complete, cex = 0.6, hang = -1, main = "Dendrogram") 

#compute distance matrix
d <- dist(df[,1:5], method = "euclidean")

#perform hierarchical clustering using the different methods
clust.avg <- hclust(d, method = "average" )
clust.single<-hclust(d, "single")
clust.complete<-hclust(d, "complete")

#cut the dendrogram into 2 clusters
groups.avg <- cutree(clust.avg, k=2)
groups.single<-cutree(clust.single, k=2)
groups.complete<-cutree(clust.complete, k=2)

hclust.avg.table<-table(df$cluster.assignment, groups.avg)
hclust.single.table<-table(df$cluster.assignment, groups.single)
hclust.complete.table<-table(df$cluster.assignment, groups.complete)

#define and display accuracy
hclust.avg.accuracy <- (hclust.avg.table[1,1] + hclust.avg.table[2,2])/200
hclust.single.accuracy <-(hclust.single.table[1,1] + hclust.single.table[2,2])/200
hclust.complete.accuracy <- (hclust.complete.table[1,1] + hclust.complete.table[2,2])/200

hclust.avg.accuracy
hclust.single.accuracy
hclust.complete.accuracy
