# PCA Reg y boxplots descriptivos
X.reg <- dataset[,c("S020","S003","X003", 
                    "Ideolog","e.ImpRelig_lin","X025R",
                    "VOmoral","VOeco")]
install.packages("factoextra")
library(factoextra)

X.reg <- na.omit(X.reg)
ctry = 32
yr = 2006

x <- X.reg[(X.reg$S003==ctry)&(X.reg$S020==yr),c("X003",
                                                 "Ideolog",
                                                 "e.ImpRelig_lin",
                                                 "X025R")]
res.pca <- prcomp(x, scale = TRUE)

fviz_eig(res.pca)
res.pca.reg <- data.frame(res.pca$x)
res.pca.reg$VOmoral <- X.reg[(X.reg$S003==ctry)&(X.reg$S020==yr),]$VOmoral
res.pca.reg$VOeco <- X.reg[(X.reg$S003==ctry)&(X.reg$S020==yr),]$VOeco



lm.1995 <- lm(VOmoral ~ PC1, 
              data=res.pca.reg)
summary(lm.1995)
par(mfrow = c(2, 2))
plot(lm.1995)

library(ggplot2)
# Boxplots
#c(32,76,170,484)

X.reg$S003 <- factor(X.reg$S003, 
                     levels = c(32,76,170,484), 
                  labels = c("Argentina", "Brasil",
                             "Colombia","Mexico"))
#32  Argentina
#76   Brazil
#170  Colombia
#484  Mexico
ggplot(X.reg, 
       aes(x=factor(S020), y=VOeco, fill=factor(Ideolog))) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus económica", 
       x = "Año",
       fill = "Ideología")
X.reg2 <- X.reg[X.reg$e.ImpRelig_lin!=1.5,]
ggplot(X.reg2, 
       aes(x=factor(S020), y=VOeco, fill=factor(e.ImpRelig_lin))) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg2$S003), scale="free")+
  labs(y = "Variable ómnibus económica", 
       x = "Año",
       fill = "Religiosidad")

ggplot(X.reg, 
       aes(x=factor(S020), y=VOeco, fill=factor(X025R))) + 
  geom_boxplot()+ scale_fill_grey()+  
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus económica", 
       x = "Año",
       fill = "Nivel educativo")
ggplot(X.reg, 
       aes(x=factor(S020), y=VOeco, fill=X003)) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus económica", 
       x = "Año",
       fill = "Edad")

#### MORAL

ggplot(X.reg, 
       aes(x=factor(S020), y=VOmoral, fill=factor(Ideolog))) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus moral", 
       x = "Año",
       fill = "Ideología")
X.reg2 <- X.reg[X.reg$e.ImpRelig_lin!=1.5,]
ggplot(X.reg2, 
       aes(x=factor(S020), y=VOmoral, fill=factor(e.ImpRelig_lin))) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg2$S003), scale="free")+
  labs(y = "Variable ómnibus moral", 
       x = "Año",
       fill = "Religiosidad")

ggplot(X.reg, 
       aes(x=factor(S020), y=VOmoral, fill=factor(X025R))) + 
  geom_boxplot()+ scale_fill_grey()+  
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus moral", 
       x = "Año",
       fill = "Nivel educativo")
ggplot(X.reg, 
       aes(x=factor(S020), y=VOmoral, fill=X003)) + 
  geom_boxplot()+ scale_fill_grey()+
  facet_wrap(factor(X.reg$S003), scale="free")+
  labs(y = "Variable ómnibus moral", 
       x = "Año",
       fill = "Edad")

