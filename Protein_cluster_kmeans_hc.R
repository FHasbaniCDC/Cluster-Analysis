#Example using Protein.csv
#Description: These data measure protein consumption in 
#25 European countries for nine food groups. 
#is possible to use multivariate methods to determine whether
#there are groupings of countries and whether meat consumption is 
#related to that of other foods.

#toInstall <- c("cluster", "fpc", "mclust")
#install.packages(toInstall, dependencies=TRUE)

#import the Protein.csv file
protein <- read.csv(file.choose(), header=TRUE)

rownames(protein) <- protein[,1] # Turn country names into observation IDs

mydata <- protein[,-1] # subset data set to just include the data you want to cluster
mydata <- na.omit(mydata) # list wise deletion of missing
mydata <- scale(mydata) # standardize variables
# mydata <-attr(mydata, "scaled:scale")
# mydata <- attr(mydata, "scaled:center")

#kmeans
library(mclust)
guess <- Mclust(mydata)
summary(guess)

plot(guess)
1
2
3
4

clusters <- 6
k_fit <- kmeans(mydata, 
                clusters,     # number of clusters
                iter.max=100, # max steps in the fitting process
                nstart=100)   # number of random starts

library(cluster)
clusplot(mydata, 
         k_fit$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=2, 
         lines=0)

#library(fpc)
plotcluster(mydata, k_fit$cluster)

clusters <- 6
clus.boot <- clusterboot(mydata, 
                         B=1000, 
                         clustermethod=kmeansCBI, # for k-means clustering
                         k=clusters, 
                         count=FALSE) # Show progress on screen?

AvgJaccard <- clus.boot$bootmean
Instability <- clus.boot$bootbrd/1000
Clusters <- c(1:clusters)
Eval <- cbind(Clusters, AvgJaccard, Instability)
Eval # AvgJaccard <0.6 is unstable & >0.85 is highly stable

#cluster 4 is unstable

k_means_clusters <- as.factor(k_fit$cluster) # First, make it a factor so that it is not confused with numeric data
country <- row.names(mydata)   # Extract row names from the working data. If we removed 
# any rows from the original data due to missingness, then
# we will need this to merge the clusters with the original data.
# Combine the two into a data frame
KMC_results <- cbind(country, k_means_clusters) # Make cluster data

# Merge the original "protein" data with the new groups
Protein_km <- merge(protein,          # Original data
                    KMC_results,    # Cluster data
                    by.x="Country",   # "Country" variable, as it appears in original data
                    by.y="country")   # "country" variable, as it appears in cluster data

#hierarchical
d <- dist(mydata, method="euclidean") # First, construct a distance matrix.

#library(cluster)
#mixed data types
#Gower scaling calculates the differences between observations on a 0 to 1.0 
#scale using a variety of comparison methods, as appropriate. 
#Qualitative, quantitative and mixed differences are calculated on the same scale, 
#resulting in a dissimilarity matrix.

# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower_d <- daisy(mydata, metric = "gower")

#A couple of options for the sake of comparison.

fit <- hclust(d, method="ward.D")   # Unmodified distances
fit2 <- hclust(d, method="ward.D2")  # Squared distances

par(mfrow=c(1,2)) # Set the plot to 1 row, 2 columns

# Plot 1
plot(fit, cex=0.4) # cex refers to font size in the plot
rect.hclust(fit, k=5) # k refers to the number of clusters

# Plot 2
plot(fit2, cex=0.4)
rect.hclust(fit, k=5)
par(mfrow=c(1,1)) # Set the plot window back to one plot

 # Assign observations to groups
clusters=5
groups <- cutree(fit, k=clusters) # Assign observations to groups
#Take a look at which countries fall into which groups.
groups

#library(cluster)
#labels=1 Identify points individually (no cluster numbers) . labels=2 Identify all points & clusters . labels=3 Identify all points only . labels=4 Identify all clusters only . labels=5 Identify individual points (w. cluster numbers)

clusplot(mydata, groups, color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)
clusters <- 5
clus.boot <- clusterboot(mydata, 
                         B=1000, # Number of bootstrap resamples
                         clustermethod=hclustCBI, # for hierarchical clustering 
                         method="ward.D", # use what we used in "hclust"
                         k=clusters, 
                         count=FALSE) # Show progress on screen?
# clus.boot
#Rule of thumb: . AvgJaccard <0.6 is unstable . AvgJaccard >0.85 is highly stable

set.seed(7239)
AvgJaccard <- clus.boot$bootmean
Instability <- clus.boot$bootbrd/1000
Clusters <- c(1:clusters)
Eval <- cbind(Clusters, AvgJaccard, Instability)
Eval

#cluster 3 is close to unstable
#okay to reconsider at this point and try a different number of clusters

hierarchical_clusters <- as.factor(groups) # First, make it a factor so that it is not confused with numeric data
country <- row.names(mydata)   # Extract row names from the working data. If we removed 
# any rows from the original data due to missingness, then
# we will need this to merge the clusters with the original data.
# Combine the two into a data frame
HC_results <- cbind(country, hierarchical_clusters) # Make cluster data

# Merge the original "protein" data with the new groups
Protein_hc<- merge(protein,          # Original data
               HC_results,     # Cluster data
               by.x="Country",   # "Country" variable, as it appears in original data
               by.y="country")   # "country" variable, as it appears in cluster data

