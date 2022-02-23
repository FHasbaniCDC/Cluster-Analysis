# load required packages
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(pmatrix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualization
  labs(subtitle = "Elbow method") # add subtitle

#The Silhouette method measures the quality of a clustering and determines 
#how well each point lies within its cluster.

# Silhouette method
fviz_nbclust(pmatrix, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


#Gap statistic method
set.seed(42)
fviz_nbclust(pmatrix, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

#The NbClust() function from the {NbClust} package, which provides 30 indices 
#for choosing the best number of clusters.
nbclust_out <- NbClust(
  data = pmatrix, #insert data name
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)
nbclust_out
fviz_nbclust(nbclust_out)
