#install.packages("factoextra")
library(factoextra)
library(cluster) 
df <- scale(pregs)

km <- kmeans(df, centers = 2, nstart = 25)

# Visualize the clusters
fviz_cluster(km, data = df)

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, 
     xlab='Number of clusters', 
     ylab='Average Silhouette Scores', 
     frame=FALSE)

fviz_nbclust(df, kmeans, method='silhouette')


install.packages("flexclust")
library(flexclust)

clu  <- cclust(dataset[,1:26], 4, dist = "euclidean", method = "hardcl",
       weights=dataset$pesos_finales, control=NULL, group=NULL, simple=FALSE,
       save.data=FALSE)
data.pca <- prcomp(dataset[,1:26], center = TRUE,scale. = TRUE)
plot(data.pca$x[,1:2], col=predict(clu))

silhouette_score <- function(k){
  km <- cclust(dataset[,1:26], k, dist = "euclidean", method = "hardcl",
               weights=dataset$pesos_finales, control=NULL, group=NULL, simple=FALSE,
               save.data=FALSE)
  ss <- silhouette(predict(km), dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, 
     xlab='Number of clusters', 
     ylab='Average Silhouette Scores', 
     frame=FALSE)

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
clu  <- cclust(dataset[,1:26], 2, dist = "euclidean", method = "hardcl",
               weights=dataset$pesos_finales, control=NULL, group=NULL, simple=FALSE,
               save.data=FALSE)
scatterplot3d(data.pca$x[,1:3], color=predict(clu))


#### SÓLO LIKERT

pregs.lik <- scale(pregs[,1:16])

fviz_nbclust(pregs.lik, kmeans, method='silhouette')

km <- kmeans(pregs.lik, centers = 2, nstart = 25)

# Visualize the clusters
fviz_cluster(km, data = pregs.lik)

#install.packages("rgl")
library(rgl)
#install.packages("car")
library(car)
likert.pca <- prcomp(pregs[,1:16], center = TRUE,scale. = TRUE)$x

scatter3d(x = likert.pca[,1], y = likert.pca[,2], 
          z = likert.pca[,3], groups = as.factor(km$cluster),
          grid = FALSE, surface = FALSE)
