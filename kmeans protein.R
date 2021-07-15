#Example using Protein.csv
#Description: These data measure protein consumption in 
#25 European countries for nine food groups. 
#is possible to use multivariate methods to determine whether
#there are groupings of countries and whether meat consumption is 
#related to that of other foods.

protein <- read.csv(file.choose())
vars.to.use <- colnames(protein)[-1]
dim(protein) #25 10


pmatrix <- scale(protein[,vars.to.use])
dim(pmatrix) #25 9
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

rm_scales <- function(scaled_matrix) {
  attr(scaled_matrix, "scaled:center") <- NULL
  attr(scaled_matrix, "scaled:scale") <- NULL
  scaled_matrix
}

pmatrix <- rm_scales(pmatrix)
km.res <- kmeans(pmatrix, 3, nstart = 25)

#Visualize
library("factoextra")
fviz_cluster(km.res, data = pmatrix,
             palette = c("#00AFBB","#2E9FDF", "#E7B800"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)
##00AFBB","#2E9FDF", "#E7B800", "#FC4E07","red"
BSS <- km.res$betweenss
TSS <- km.res$totss
# We calculate the quality of the partition
BSS / TSS * 100
#The quality of the partition is 66.69%.

km.res2 <- kmeans(pmatrix, centers = 5, nstart = 10)
100 * km.res2$betweenss / km.res2$totss

# load required packages
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(pmatrix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

# Silhouette method
fviz_nbclust(pmatrix, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

set.seed(42)
fviz_nbclust(pmatrix, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

#The optimal number of clusters is the one that maximizes the gap statistic. 
#This method suggests only 1 cluster (which is therefore a useless clustering).

#As you can see these three methods do not necessarily lead to the same result. 
#Here, all 3 approaches suggest a different number of clusters.

#NbClust()
#provides 30 indices for choosing the best number of clusters.
#install.packages("NbClust")
library(NbClust)
nbclust_out <- NbClust(
  data = pmatrix,
  distance = "euclidean",
  min.nc = 4, # minimum number of clusters
  max.nc = 11, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 6 clusters
nbclust_plot <- subset(nbclust_plot, clusters >=2 & clusters <= 6)

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

library(cluster)

set.seed(42)
km_res <- kmeans(pmatrix, centers = 2, nstart = 20)

sil <- silhouette(km_res$cluster, dist(pmatrix))
fviz_silhouette(sil)

#theinterpretation of the silhouette coefficient is as follows:
# > 0 means that the observation is well grouped. The closer the coefficient 
#is to 1, the better the observation is grouped.
#< 0 means that the observation has been placed in the wrong cluster.
#= 0 means that the observation is between two clusters.

library(factoextra)
fviz_cluster(km_res, pmatrix, ellipse.type = "norm")
