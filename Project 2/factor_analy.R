#https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html

help("factanal")

X.fact <- dataset[,6:15]

consis.fa <- factanal(X.fact[dataset$km.cluster.lik==2,], factors = 3)
consis.fa

loadings <- consis.fa$loadings
loadings <- as.data.frame(loadings[,])
loadings$pregs <- rownames(loadings)
loadings.m <- melt(loadings,id="pregs",
                   variable.name="Factor", value.name="Loading")

ggplot(loadings.m, aes(pregs, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  xlab("Pregunta")+
  theme_bw(base_size=10) #use a black-and0white theme with set font size


X.fact <- dataset[,6:20]

consis.fa <- factanal(X.fact, factors = 5)
consis.fa

loadings <- consis.fa$loadings
loadings <- as.data.frame(loadings[,])
loadings$pregs <- rownames(loadings)
loadings.m <- melt(loadings,id="pregs",
                   variable.name="Factor", value.name="Loading")

ggplot(loadings.m, aes(pregs, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  xlab("Pregunta")+
  theme_bw(base_size=10) #use a black-and0white theme with set font size



consis.fa$uniquenesses
#The uniqueness, sometimes referred to as noise, 
#corresponds to the proportion of variability, which can not be explained by a linear combination of the factors

apply(consis.fa$loadings^2,1,sum) # communality



plot(consis.fa$loadings[,1], 
     consis.fa$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1.1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(consis.fa$loadings[,1]-0.08, 
     consis.fa$loadings[,2]+0.08,
     colnames(X.fact),
     col="blue")



ggplot(loadings.m, aes(Test, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and-white theme with set font size