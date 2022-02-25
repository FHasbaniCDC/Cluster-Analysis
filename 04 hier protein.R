load("Energy.RData")

#Example using Protein.csv
#Description: These data measure protein consumption in 
#25 European countries for nine food groups. 
#is possible to use multivariate methods to determine whether
#there are groupings of countries and whether meat consumption is 
#related to that of other foods.
protein <- read.csv(file.choose())
View(protein)

pairs(protein[, 2:10], main="Pairs Plot of Protein Variables")

#variables to use for clustering
vars.to.use <- colnames(protein)[-1]
#[,] if wanted all rows and columns 2:3 [,2:3]

pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

rm_scales <- function(scaled_matrix) {
  attr(scaled_matrix, "scaled:center") <- NULL
  attr(scaled_matrix, "scaled:scale") <- NULL
  scaled_matrix
}

pmatrix <- rm_scales(pmatrix)
#kmeans 

#hierarchial
#calculate a distance measure, a value that quantifies the pairwise differences of all samples in your dataset, a value that quantifies the pairwise differences of all samples in your dataset. 

#The most common distance metric is "Euclidean", the shortest distance between two samples. Another common metric is the "Manhattan" distance which can emphasize difference between samples.

#create distance matrix
distmat <- dist(pmatrix, method="euclidean")  
#Perform clustering and visualize the result as a dendrogram.
pfit <- hclust(distmat,method="ward.D")

pfit$labels <- paste(protein$Country, rownames(protein))

plot(pfit, cex=0.75, main="Clustering of Protein Measurement Variables")

#alternative cluster plot
#plot(pfit, labels=protein$Country)

#Perform clustering and visualize the result as a dendrogram.

rect.hclust(pfit, k=5) #creating red boxes around clusters

library(RColorBrewer)
protein.cols <- brewer.pal(25, "Set3")
sam.cols <- protein.cols[as.factor(protein$Country)]

plot(as.dendrogram(pfit), type="rectangle", leaflab="none", cex.lab=2, main="Clustering of Protein Measures Variables")

mtext(protein$Country[pfit$order], col=sam.cols[pfit$order], side=1, at=1:nrow(protein), line=0.5, cex=0.5, las=2, family="mono")


pairs(protein[, 2:10], col=sam.cols, main="Pairs Plot of Variables")

##To extract the members of each cluster from the hclust object, use cutree().

#There's a certain logic to these clusters: the countries in each 
#cluster tend to be in the same geographical region. It makes sense that countries 
#in the same region would have similar dietary habits. You can also see that

#extract the clusters found using hclust()
#convenience function for printing out the countries in each cluster.
#function is hardcoded for protein dataset
groups <- cutree(pfit, k=5)

print_clusters <- function(data, groups, columns){
  groupedD <- split(data,groups)
  lapply(groupedD, 
         function(df) df[,columns])
}

#install.packages("wrapr")
library(wrapr)
cols_to_print <- wrapr::qc(Country, RedMeat, Fish, Fruit_Vegetables, Milk, Starch)
print_clusters(protein, groups, cols_to_print)

#visualizing clusters
#use the prcomp() call to do the principal components decomposition.
library(ggplot2)
princ <- prcomp(pmatrix) #PCA
nComp <- 2
# rotate the data into the space described by the principal components. We only want the projection on the first two axes.
project <- predict(princ, newdata=pmatrix)[,1:nComp]

#Create a data frame with the transformed data,  
project_plus <- cbind(as.data.frame(project),
                cluster=as.factor(groups),
                country=protein$Country)

ggplot(project_plus, aes(x=PC1, y=PC2)) + 
   geom_point(data = as.data.frame(project), color = "darkgrey") +
   geom_point() +
   geom_text(aes(label = country),
            hjust = 0, vjust = 1) +
   facet_wrap(~cluster, ncol = 3, labeller = label_both)
#Bootstrap evaluation of clusters
#An important question when evaluating clusters is whether a given
#cluster is "real"-does the cluster represent actual structure in the data, 
#or is it an artifact of the clustering algorithm?
#important with clustering algorithms like k-means, 
#where the user has to specify the number of clusters a priori.
#One way to assess whether a cluster represents true structure is to see if the 
#cluster holds up under plausible variations in the dataset. The fpc package has
#a function called clusterboot() that uses bootstrap resampling to evaluate how 
#stable a given cluster is. clusterboot() is an integrated function that both 
#performs the clustering and evaluates the final produced clusters. It has interfaces
#to a number of R clustering algorithms, including both hclust and kmeans.
  
#clusterboot's algorithm uses the Jaccard coefficient, a similarity measure between sets. 
#install.packages("fpc")
#Jaccard = (intersection/union)*55%

library(fpc)
kbest.p <- 5 #set number of desire clusters
  
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, method='ward.D', k=kbest.p)
summary(cboot.hclust$result)

#The results of the clustering are in cboot.hclust$result. 
#The output of the hclust() function is in cboot.hclust$result$result. 

groups<-cboot.hclust$result$partition #returns a vector of cluster labels
#syntax - print_clusters(groups,kbest.p) 

print_clusters(protein,groups,cols_to_print)
cboot.hclust$bootmean #  The vector of cluster stabilities.

#count of how many clusters were dissolved (default=100 bootstrap iterations)
cboot.hclust$bootbrd 

#clusterboot() assumes that you know the number of clusters, k. 
#eye-balled from dendrogram

#picking the number of clusters
#total wss
#total within sum of squares (WSS) for different values of k and look for an "elbow" in the curve. Define the cluster's centroid as the point that is the mean value of all the points in the cluster. 
sqr_edist <- function(x,y) {
    sum((x-y)^2)
}

#Function to calculate the WSS for a single cluster, which is represented as a matrix 
#Calculate the centroid of the cluster
wss.cluster <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply(clustermat, 1, FUN = function(row) { sqr_edist(row, c0) }))
}

wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels == i))
  wsstot
}

wss.total(pmatrix, groups)

get.wss <- function(dmatrix, max_clusters) {
  wss = numeric(max_clusters)
  wss[1] <- wss.cluster(dmatrix)
  
  d <- dist(dmatrix, method = "euclidean")
  pfit <- hclust(d, method = "ward.D")
  
  for(k in 2:max_clusters) {
    
    labels <- cutree(pfit, k = k)
    wss[k] <- wss.total(dmatrix, labels)
  }
   wss
}

kmax <- 10
cluster_meas <- data.frame(nclusters = 1:kmax,
                wss = get.wss(pmatrix, kmax))

breaks <- 1:kmax
ggplot(cluster_meas, aes(x=nclusters, y = wss)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = breaks)

###############################################
#kmeans clustering

kbest_p <- 5
pclusters <-kmeans(pmatrix,kbest_p,nstart=100, iter.max=100)
summary(pclusters)

pclusters$centers
pclusters$size

groups <- pclusters$cluster
cols_to_print = wrapr::qc(Country, RedMeat, Fish, Fruit_Vegetables)
print_clusters(protein, groups, cols_to_print)

#The kmeansruns() function for picking k one of "asw" or "ch". Determines whether average silhouette width or Calinski-Harabasz is applied.

#internal cluster validation index:
#Compute the Calinski and Harabasz score. It is also known as the Variance Ratio Criterion. The score is defined as ratio between the within-cluster dispersion and the between-cluster dispersion.
#Higher value of CH index means the clusters are dense and well separated, although there is no "acceptable" cut-off value. 
clustering_ch <- kmeansruns(pmatrix, krange = 1:10, criterion = "ch")

clustering_ch$bestk
## [1] 2

clustering_asw <- kmeansruns(pmatrix, krange = 1:10, criterion = "asw")

#cluster - A vector of integers indicating the cluster to which each point is allocated.
#centers - A matrix of cluster centers.
#withinss	- The within-cluster sum of squares for each cluster.
#size - The number of points in each cluster.
#bestk- The optimal number of clusters.
#crit- Vector with values of the criterion for all used numbers of clusters (0 if number not tried).

clustering_asw$bestk
## [1] 3
clustering_asw$centers #type clustering_asw$
clustering_asw$crit
## [1] 0.0000000 0.3271084 0.3351694 0.2617868 0.2639450 0.2734815 0.2471165
## [8] 0.2429985 0.2412922 0.2388293

clustering_ch$crit
##  [1]  0.000000 14.094814 11.417985 10.418801 10.011797  9.964967  9.861682
##  [8]  9.412089  9.166676  9.075569

cluster_meas$wss
cluster_meas$nclusters

summary(clustering_ch)

##              Length Class  Mode
## cluster      25     -none- numeric
## centers      18     -none- numeric
## totss         1     -none- numeric
## withinss      2     -none- numeric
## tot.withinss  1     -none- numeric
## betweenss     1     -none- numeric
## size          2     -none- numeric
## iter          1     -none- numeric
## ifault        1     -none- numeric
## crit         10     -none- numeric
## bestk         1     -none- numeric
#example using summary parts
clustering_ch$centers

kbest_p <- 5
#Clusterwise cluster stability assessment by resampling
cboot <- clusterboot(pmatrix, clustermethod = kmeansCBI,
                     runs = 100,iter.max = 100,
                     krange = kbest_p, seed = 15555)

groups <- cboot$result$partition
print_clusters(protein, groups, cols_to_print)
## $`1`
##       Country RedMeat Fish Fruits_Vegetables
## 1     Albania    10.1  0.2    1.7
## 4    Bulgaria     7.8  1.2    4.2
## 18    Romania     6.2  1.0    2.8
## 25 Yugoslavia     4.4  0.6    3.2
##
## $`2`
##    Country RedMeat Fish Fruits_Vegetables
## 6  Denmark    10.6  9.9    2.4
## 8  Finland     9.5  5.8    1.4
## 15  Norway     9.4  9.7    2.7
## 20  Sweden     9.9  7.5    2.0
##
## $`3`
##           Country RedMeat Fish Fruits_Vegetables
## 5  Czechoslovakia     9.7  2.0    4.0
## 7       E Germany     8.4  5.4    3.6
## 11        Hungary     5.3  0.3    4.2
## 16         Poland     6.9  3.0    6.6
## 23           USSR     9.3  3.0    2.9
##
## $`4`
##        Country RedMeat Fish Fruits_Vegetables
## 2      Austria     8.9  2.1    4.3
## 3      Belgium    13.5  4.5    4.0
## 9       France    18.0  5.7    6.5
## 12     Ireland    13.9  2.2    2.9
## 14 Netherlands     9.5  2.5    3.7
## 21 Switzerland    13.1  2.3    4.9
## 22          UK    17.4  4.3    3.3
## 24   W Germany    11.4  3.4    3.8
##
## $`5`
##     Country RedMeat Fish Fruits_Vegetables
## 10   Greece    10.2  5.9    6.5
## 13    Italy     9.0  3.4    6.7
## 17 Portugal     6.2 14.2    7.9
## 19    Spain     7.1  7.0    7.2

cboot$bootmean
## [1] 0.8670000 0.8420714 0.6147024 0.7647341 0.7508333

cboot$bootbrd
## [1] 15 20 49 17 32

