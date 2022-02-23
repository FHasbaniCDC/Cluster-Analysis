#kmeans
data = read.csv(file.choose())
data = read.csv("Exercise_Spend.csv")
head(data)

colnames(data) = c("CustomerID", "Gender", "Age", "Annual_Income", "Spending_Score")
summary(data[,c(3:5)])

box = boxplot(data[, 3:5], col = "lightblue")
#Age is left-skewed, i.e., we are having 50 percent customers of young age ([18, 36] years of age).
#Annual_Income is right-skewed 
#Spending_Score is left-skewed. Annual_Income column contains outliers. 

box$stats
#column 2 contains the boxplot stats for Annual_Income. #calculate quantiles and explore

quantile(data$Annual_Income, seq(0, 1, 0.02)) #from, to, by

data$Annual_Income = ifelse(data$Annual_Income>=126, 126, data$Annual_Income)
                            
#do the boxplots again
box = boxplot(data[,3:5], col = "orange")

#dummy variable
data$IsMale = ifelse(data$Gender=="Male", 1, 0)
data$IsFemale = ifelse(data$Gender=="Female", 1, 0)
# Excluding Gender and Customer ID
data = data[, 3:ncol(data)]
head(data)

#no outliers are there in our data
# using min-max scaling in our case.
maxs <- apply(data, 2, max) #sort colns by max
mins <- apply(data, 2, min) #sort colns by min
#Min-max scaling is similar to z-score normalization in that it will replace every value in a column with a new value using a formula. 
data_sc = scale(data, center = mins, scale = maxs - mins)
head(data_sc)

library(factoextra)
library(ggplot2)
library(cluster)

dev.off()
#within sum of squares plot 
fviz_nbclust(data_sc, kmeans, method ="wss") + labs(subtitle = "wss method")

fviz_nbclust(data_sc, kmeans, method ="wss") +  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "wss method")

set.seed(711)
k_means_results <- kmeans(data_sc, 2, nstart = 25)

#advised to choose large values of nstart = 25 to 50 for better stability in 
#clustering. By default, nstart = 1. nstart is important because this number 
#determines the number of random assignments chosen at the starting of the algorithm.
#The algorithm will then choose only those assignments at the end which have lowest
#wss corresponding to them.

# Printing the results
print(k_means_results)

#means of different variables are corresponding to each cluster
aggregate(data,by=list(cluster=k_means_results$cluster), mean)

data$Clusters = k_means_results$cluster

k_means_results$cluster
#checking size of the clusters

#Accessing the centers of the clusters.
k_means_results$centers
k_means_results$size

fviz_cluster(k_means_results,data =data,palette =c("#00AFBB", "#FC4E07"),
             ellipse.type ="euclid",
             star.plot =FALSE,
             repel =TRUE,
             ggtheme =theme_minimal()
)

#thoughts?
#kmeans requires input of number of clusters is required to be inputted first into the function.
#After that only, the algorithm can proceed. use WSS plot.

#kmeans highly sensitive to outliers. If lots of outliers, use PAM algorithm.
#kmeans final result depends heavily on the value provided to nstart. Start high 25-50.

#pam
#In order to create the distance matrix in PAM, we can use the following two distances:
  
#histogram
library(psych)
multi.hist(data,nrow = 3, ncol=2,density=TRUE ,freq=FALSE,bcol="lightblue",
dcol= c("red","blue"),dlty=c("solid", "dotted"), main=colnames(data)) 

#Annual_Income and Age are left skewed

# main parameters - pam(x, k,metric ="euclidean",stand =FALSE)
#silhouette analysis.
scaleddata = scale(data)
fviz_nbclust(scaleddata, pam, method ="silhouette")+theme_minimal()

#k=2
set.seed(777)
pamResult <-pam(scaleddata, k = 2)
pamResult

#added cluster data, clusterp to df
data$clusterp = pamResult$cluster
head(data)

#pam function results and object of class pam which has two most important 
#components: * medoids * clustering
pamResult$medoids #objects that represent clusters

pamResult$clusinfo
pamResult$silinfo
library(ggrepel)
fviz_cluster(pamResult, 
             palette =c("#007892","#D9455F"),
             #ellipse.type ="euclid",
             repel =TRUE,
             ggtheme =theme_minimal())

