library(tidyverse)
library(dbscan)
library(plotly)
set.seed(1234)

##############################
########  DBSCAN  ############
##############################

df<-read.csv("simulatedS.csv")
head(df)
dim(df)

# Assign predictor and predicted columns
col.predict<-as.data.frame(df[,1:3])

# Set parameters for DBScan
mult<- 2
minpoints<-round(ncol(col.predict)*mult)
k<-minpoints-1

# Look at graph to set epsilon=.13
dev.new()
kNNdistplot(col.predict, k = k)
abline(h=.13, col="red", lty="dashed")

# Run dbscan
df_dbscan <- dbscan(x = col.predict, 
                    minPts = minpoints, 
                    eps = .13, 
                    borderPoints = T)
df_dbscan

fig <- plot_ly(df, x = ~df[,1], y = ~df[,2], z = ~df[,3], color = df_dbscan$cluster)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Z')))

fig


# Look at PC plot of dbscan groupings
dev.new()
hullplot(col.predict, df_dbscan, 
         cex = 2, pch = col.response)

legend("topright",                    # Add legend to plot
       legend = c("Cluster 1", 
                  "Cluster 2"),
       pch = c(1,2))
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

