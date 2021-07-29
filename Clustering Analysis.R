#Get your data into R:
# Use the R code below. You will be asked to choose a file:
# .txt file: Read tab separated values
my_data <- read.delim(file.choose())
# .csv file: Read comma (",") separated values
my_data <- read.csv(file.choose())
# .csv file: Read semicolon (";") separated values
my_data <- read.csv2(file.choose())

#Data Preparation
#data should be prepared as follow:
#Rows are observations (individuals) and columns are variables
#Any missing value in the data must be removed or estimated.
#The data must be standardized (i.e., scaled) to make variables comparable. Recall
#that, standardization consists of transforming the variables such that they have
#mean zero and standard deviation one.

#create a dataframe and scale

#install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)

View(protein)
#kmeans
pdata<-protein[,-1]
km.resp<-kmeans(pdata,5)
#We enter the results of k-means as the first parameter.
#In data, enter the data on which clustering was done. 
#In pallete, select the type of the geometry of points, and 
#in ggtheme, select the theme of the output plot
fviz_cluster(km.resp, data = pdata,palette = "jco", ggtheme = theme_minimal())

#k-medoids
#Partitioning Around Medoids - PAM 
km<-pam(pdata,5)
fviz_cluster(km, data = pdata,palette = "jco",ggtheme = theme_minimal())

#comparing kmeans and kmedoids using protein

pair_dis<-daisy(pdata)
sc<-silhouette(km.resp$cluster, pair_dis)
plot(sc,col=1:8,border=NA)

#Plot a graph of the silhouette score versus number of clusters (up to 20):

fviz_nbclust(pata, kmeans, method = "silhouette",k.max=20)
########################################################
View(USArrests)
vars.to.use <- colnames(USArrests)
amatrix <- scale(USArrests[,vars.to.use])
acenter <- attr(amatrix, "scaled:center")
ascale <- attr(amatrix, "scaled:scale")

rm_scales <- function(scaled_matrix) {
  attr(scaled_matrix, "scaled:center") <- NULL
  attr(scaled_matrix, "scaled:scale") <- NULL
  scaled_matrix
}

amatrix <- rm_scales(amatrix)
k3 <- kmeans(amatrix, centers = 3, nstart = 20)
str(k3)
k3

fviz_cluster(k3, data = k3)
install.packages("NbClust")
library(NbClust)
fviz_nbclust(k3, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "Elbow method")


######################kmeans

#cluster: for cluster analyses and
#factoextra: for the visualization of the analysis results.

#install.packages(c("cluster", "factoextra"))

#Assessing clustering tendency (i.e., the clusterability of the data)
#Defining the optimal number of clusters
#Computing partitioning cluster analyses (e.g.: k-means, pam) or hierarchical clustering
#Validating clustering analyses: silhouette plot


#Cluster validation statistics: Inspect cluster silhouette plot
#eclust(): Enhanced clustering analysis
#K-means clustering using eclust()
#Hierachical clustering using eclust()

# Load the data set
data(USArrests)
# Standardize
df <- scale(USArrests)
#Assessing the clusterability
#The function get_clust_tendency() [factoextra package] can be used. 
#It computes the Hopkins statistic and provides a visual approach.
res <- get_clust_tendency(df, 40, graph = TRUE)
# Hopskin statistic
res$hopkins_stat

#The value of the Hopkins statistic is significantly < 0.5, 
#indicating that the data is highly clusterable. Additionally, 
#It can be seen that the ordered dissimilarity image contains patterns (i.e., clusters).

# Visualize the dissimilarity matrix
print(res$plot)

#Estimate the number of clusters in the data
set.seed(7112)
# Compute the gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 100) 
# Plot the result
library(factoextra)
fviz_gap_stat(gap_stat)

#K-means clustering with k = 4:
#Compute k-means
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
head(km.res$cluster, 20)

#Visualize clusters using factoextra
fviz_cluster(km.res, USArrests)

#Cluster validation statistics: Inspect cluster silhouette plot
sil <- silhouette(km.res$cluster, dist(df))
fviz_silhouette(sil)
silinfo <- km.res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km.res$size




#cluster versus those in the neighbor cluster. Si values range from 1 to - 1:
#Silh measures how similar object i is to the other objects in its own cluster
#versus those in the neighbor cluster, S1 ranges from 1 to -1
# Si close to 1 indicates that the object is well clustered. The object i is similar to the other objects in its group.
# Si close to -1 indicates that the object is poorly clustered, and that assignment to some other cluster would 
  #probably improve the overall results.

sil <- silhouette(km.res$cluster, dist(df))
rownames(sil) <- rownames(USArrests)
head(sil[, 1:3])

fviz_silhouette(sil)

#some samples have negative silhouette values. Ask:
#Which samples are these? To what cluster are they closer?
#This can be determined from the output of the function silhouette():
  
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

#validation statistics
#The function cluster.stats() [fpc package] and the function NbClust() 
#[in NbClust package] can be used to compute Dunn index 
#and many other cluster validation statistics or indices.
#The simplified format is: cluster.stats(d = NULL, clustering, al.clustering = NULL)

cluster.number: number of clusters
cluster.size: vector containing the number of points in each cluster
average.distance, median.distance: vector containing the cluster-wise within
average/median distances
average.between: average distance between clusters. We want it to be as large as possible
average.within: average distance within clusters. We want it to be as small as possible
clus.avg.silwidths: vector of cluster average silhouette widths. Recall that, the silhouette width is also an estimate of the average distance between clusters. Its value is comprised between 1 and -1 with a value of 1 indicating a very good cluster.
within.cluster.ss: a generalization of the within clusters sum of squares (k-means objective function), which is obtained if d is a Euclidean distance matrix.
dunn, dunn2: Dunn index
corrected.rand, vi: Two indexes to assess the similarity of two clustering: the corrected Rand index and Meila's VI
All the above elements can be used to evaluate the internal quality of clustering.

In the following sections, we'll compute the clustering quality statistics for k-means. Look at the within.cluster.ss (within clusters sum of squares), the average.within (average distance within clusters) and clus.avg.silwidths (vector of cluster average silhouette widths).

library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),  km.res$cluster)
# Dun index
km_stats$dunn
## [1] 0.0265
To display all statistics, type this:
  
  km_stats
Read the documentation of cluster.stats() for details about all the available indices.

External clustering validation
Among the values returned by the function cluster.stats(), there are two indexes to assess the similarity of two clustering, namely the corrected Rand index and Meila's VI.

We know that the iris data contains exactly 3 groups of species.

Does the K-means clustering matches with the true structure of the data?
  
  We can use the function cluster.stats() to answer to this question.

Let start by computing a cross-tabulation between k-means clusters and the reference Species labels:
  
  table(iris$Species, km.res$cluster)
##             
##               1  2  3
##   setosa     50  0  0
##   versicolor  0 11 39
##   virginica   0 36 14
It can be seen that:
  
  All setosa species (n = 50) has been classified in cluster 1
A large number of versicor species (n = 39 ) has been classified in cluster 3. Some of them ( n = 11) have been classified in cluster 2.
A large number of virginica species (n = 36 ) has been classified in cluster 2. Some of them (n = 14) have been classified in cluster 3.
It's possible to quantify the agreement between Species and k-means clusters using either the corrected Rand index and Meila's VI provided as follow:
  
  library("fpc")
# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df), 
                             species, km.res$cluster)
# Corrected Rand index
clust_stats$corrected.rand
## [1] 0.62
# VI
clust_stats$vi
## [1] 0.748
The corrected Rand index provides a measure for assessing the similarity between two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement). Agreement between the specie types and the cluster solution is 0.62 using Rand index and 0.748 using Meila's VI.

The same analysis can be computed for both PAM and hierarchical clustering:
  
  # Agreement between species and pam clusters
  pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
table(iris$Species, pam.res$cluster)
cluster.stats(d = dist(iris.scaled), 
              species, pam.res$cluster)$vi
# Agreement between species and HC clusters
res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
cluster.stats(d = dist(iris.scaled), 
              species, res.hc$cluster)$vi
External clustering validation, can be used to select suitable clustering algorithm for a given data set.

Summary
We described how to validate clustering results using the silhouette method and the Dunn index. This task is facilitated using the combination of two R functions: eclust() and fviz_silhouette in the factoextra package We also demonstrated how to assess the agreement between a clustering result and an external reference.
In the next chapters, we'll show how to i) choose the appropriate clustering algorithm for your data; and ii) computing p-values for hierarchical clustering.






#continue with eclust 