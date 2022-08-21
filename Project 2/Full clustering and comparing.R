# Full clustering and comparing
library(gtsummary)
library(umap)
library(ggplot2)
library(cluster) 
library(factoextra)
library(dplyr)

dataset <- read.csv("dataset_regla1.csv")

dataset$P22.factor <- factor(dataset$P22, 
                             labels = c("Macri","Fernandez",
                                        "Lavagna", "Otro"))
dataset$P21.factor <- factor(dataset$P21, levels = c('1','2','3',
                                                     '4','5','6','7',
                                                     '8','9'),
                             labels = c("Macri",
                                        "Fernandez",
                                        "Larreta",
                                        "Bullrich",
                                        "Kirchner",
                                        "Milei",
                                        "Massa",
                                        "Otro",
                                        "NsNc"
                             ))
dataset <- dataset[,2:31]

likert.cols <- c(6:20)
catego.cols <- c(21:24)

km.likert <- kmeans(scale(dataset[,likert.cols]), centers = 2, nstart = 25)
fviz_cluster(km.likert, data = scale(dataset[,likert.cols]))
dataset$km.cluster.lik <- km.likert$cluster

km.catego <- kmeans(scale(dataset[,catego.cols]), centers = 2, nstart = 25)
fviz_cluster(km.catego, data = scale(dataset[,catego.cols]))
dataset$km.cluster.cat <- km.catego$cluster

#install.packages("pdfCluster")
library(pdfCluster)
adj.rand.index(dataset$km.cluster.cat, dataset$km.cluster.lik)
dataset$km.cluster.cat <- ifelse(
  dataset$km.cluster.cat==1,2,1)

set.seed(102)
dat.umap = umap(dataset[,c(likert.cols, catego.cols)], 
                n_neighbors=10, preserve.seed = TRUE)
layout.umap <- dat.umap$layout
layout.umap <- as.data.frame(layout.umap)
colnames(layout.umap) <- c("UMAP1","UMAP2")
dataset$UMAP1 <- layout.umap$UMAP1
dataset$UMAP2 <- layout.umap$UMAP2

ggplot(aes(x=UMAP1, y=UMAP2), data=dataset)+
  geom_point()+
  labs(title = "")

ggplot(aes(x=UMAP1, y=UMAP2, color= km.cluster.cat), data=dataset)+
  geom_point()+
  labs(title = "")

ggplot(aes(x=UMAP1, y=UMAP2, color= km.cluster.lik), data=dataset)+
  geom_point()+
  labs(title = "")

table(dataset[,c(31,32)])
(337+279)/(337+279+118+172)

scaled.dataset.all <- as.data.frame(
  scale(dataset[,6:24])
)
dat.umap = umap(scaled.dataset.all, 
                n_neighbors=30, preserve.seed = TRUE)
layout.umap <- dat.umap$layout
layout.umap <- as.data.frame(layout.umap)
colnames(layout.umap) <- c("UMAP1","UMAP2")
dataset$UMAP1.sc <- layout.umap$UMAP1
dataset$UMAP2.sc <- layout.umap$UMAP2

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc), data=dataset)+
  geom_point()+
  labs(title = "")

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc, color= km.cluster.lik), data=dataset)+
  geom_point()+
  labs(title = "")

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc), data=dataset)+
  geom_point(aes(shape=factor(km.cluster.lik), color=factor(km.cluster.lik)))+
  stat_density2d()+
  labs(title = "Elecciones próximo mes y kmeans cluster")+
  facet_wrap(dataset$P21.factor)

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc), data=dataset)+
  geom_point(aes(shape=factor(km.cluster.lik), color=factor(km.cluster.lik)))+
  stat_density2d()+
  labs(title = "Elecciones 2019 y kmeans cluster")+
  facet_wrap(dataset$P22.factor)

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc), data=dataset)+
  geom_point(aes(shape=factor(km.cluster.lik), color=factor(km.cluster.cat)))+
  stat_density2d()+
  labs(title = "Elecciones próximo mes y kmeans cluster")+
  facet_wrap(dataset$P21.factor)

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc), data=dataset)+
  geom_point(aes(shape=factor(km.cluster.lik), color=factor(km.cluster.cat)))+
  stat_density2d()+
  labs(title = "Elecciones 2019 y kmeans cluster")+
  facet_wrap(dataset$P22.factor)


# CLusters pero con variables categóricas también

km.likert <- kmeans(scale(dataset[,c(1:5,likert.cols)]), centers = 2, nstart = 25)
fviz_cluster(km.likert, data = scale(dataset[,c(1:5,likert.cols)]))
dataset$km.cluster.lik <- km.likert$cluster

km.catego <- kmeans(scale(dataset[,c(1:5,catego.cols)]), centers = 2, nstart = 25)
fviz_cluster(km.catego, data = scale(dataset[,c(1:5,catego.cols)]))
dataset$km.cluster.cat <- km.catego$cluster
dataset$km.cluster.cat <- 3-dataset$km.cluster.cat

table(dataset[,c(31,32)])
(337+279)/(337+279+118+172) #antes de variables 1:5
(267+347)/(267+347+172+120)

ggplot(aes(x=UMAP1.sc, y=UMAP2.sc, color= km.cluster.cat), data=dataset)+
  geom_point()+
  labs(title = "")
