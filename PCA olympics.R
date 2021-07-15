#Data: decathlon2 [in factoextra package]
#PCA function: FactoMineR::PCA()
#Visualization factoextra::fviz_pca()
#Ensure to have factoextra and factoMineR installed

library("factoextra")
data("decathlon2")
df <- decathlon2[1:23, 1:10]

library("FactoMineR")
res.pca <- PCA(df,  graph = FALSE)

# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(res.pca)
var

# Coordinates of variables
head(var$coord)

# Contribution of variables
head(var$contrib)

# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")

#It's possible to control variable colors using their contributions 
#("contrib") to the principal axes:
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

#Variable contributions to the principal axes:
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)
