data() #finding loaded datasets
data("USArrests")
str(USArrests)
View(USArrests)

df <- scale(USArrests)
head(df)

#kmeans is in the [stats package]
library(factoextra)
library(ggplot2)

#finding the optimal k using fviz_nclust function
fviz_nbclust(df, kmeans,method="wss") + geom_vline(xintercept = 4, linetype=2)

#compute kmeans with k=4
set.seed(100)
km.res<- kmeans(df, 4, nstart=25)
print(km.res)
km.res$centers
km.res$size

aggregate(USArrests, by=list(cluster=km.res$cluster),mean)
View(USArrests)

dd<- cbind(USArrests, cluster=km.res$cluster)
head(dd)

fviz_cluster(km.res, data=df,
             palette=c("yellow","red","blue","green"),
             ellipse.type = "euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())
#quality of a k-means partition is found by calculating the percentage of 
#the TSS "explained". the higher, the better, BSS is large and/or WSS is small.

# We calculate the quality of the partition
#BSS / TSS * 100

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(42)
fviz_nbclust(df, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

#computes the NmClust() for kmeans
nbclust_out <- NbClust(
  data = df,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)
fviz_nbclust(nbclust_out)

# Compute PAM
#install.packages(c("fpc","cluster")
library(fpc)
library("cluster")

km.res <- kmeans(df,4)
km.res$centers
pam.res <- pam(df, 4)
pam.res$clustering  
pam.res$clusinfo #play with $ to see all components

# Visualize
fviz_cluster(km.res)
fviz_cluster(pam.res)