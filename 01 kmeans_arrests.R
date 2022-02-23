

library(factoextra)
library(ggplot2)
library(skimr)
library(fpc)
library(cluster)

#Violent Crime Rates by US State Description. This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also given is the percent of the population living in urban areas.
data("USArrests")
View(USArrests) 

#explore structure
str(USArrests)
skim(USArrests)

#remove rows with missing values
df <- na.omit(df)

#Clustering requires scaling data
df <- scale(df)
head(df)

#Determining and Visualizing the Optimal Number of Clusters
#finding the optimal k using fviz_nclust function
#method = c("silhouette", "wss", "gap_stat")
fviz_nbclust(df, kmeans, method="wss") + 
  geom_vline(xintercept = 4, linetype=2)

#kmeans is in the [stats package]
#compute kmeans with k=4
set.seed(100)
km.res<- kmeans(df, 4, nstart=25) #k and number of centers chosen

print(km.res) 
#quality of a k-means partition is found by calculating the percentage of the TSS "explained". the higher, the better, BSS is large and/or WSS is small. Calculate the quality of the partition as 
#BSS / TSS * 100

km.res$centers #cluster centers
km.res$size #number of obs in each cluster

#plot results of final k-means model
fviz_cluster(km.res, data = df)

#find means of each cluster
aggregate(USArrests, by=list(cluster=km.res$cluster),mean)
#i.e. The mean number of murders per 100,000 citizens among the states in cluster 1 is 3.6.

View(USArrests)

#add cluster assignment to original data
dd<- cbind(USArrests, cluster=km.res$cluster)
head(dd) #view final dataset

#Visualize Clustering Results using fviz_cluster
fviz_cluster(km.res, data=df,
             palette=c("yellow","red","blue","green"),
             ellipse.type = "euclid", #frame type
             star.plot=TRUE,
             repel=TRUE, #avoids overplotting text
             ggtheme=theme_minimal())

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

# Gap statistic: Another way to determine the optimal number of clusters. compares the total intra-cluster variation for different values of k with their expected values for a distribution with no clustering.
set.seed(42)
fviz_nbclust(df, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

#Another option is from the cluster package, clusGap


library(NbClust)
#computes the NbClust() for kmeans, from NbClust package
nbclust_out <- NbClust(
  data = df,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)
fviz_nbclust(nbclust_out) #factoextra package

# Compute PAM
km.res <- kmeans(df,4)
km.res$centers

pam.res <- pam(df, 4)
pam.res$clustering  
pam.res$clusinfo #play with $ to see all components

# Visualize
fviz_cluster(pam.res)
