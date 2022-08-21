# MCLUST
# https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html

#install.packages("mclust")
library(mclust)

X <- df[(df$S003==32)&(df$S020==1991), ]

fit = Mclust(X$VO, G=1)
summary(fit)
plot(fit, what = "density", data = X$VO, breaks = 15)
plot(fit, what = "density", type = "persp")

fit$loglik

mod4 <- densityMclust(X$VO)
summary(mod4)
plot(mod4, what = "BIC")

fit = Mclust(X$VO, G=2)
summary(fit)
plot(fit, what = "density", data = X$VO, breaks = 15)


unique(df$S020)
#1981, 1982,1983,1984,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,
#2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015
#2016,2017,2018,2019,2020

X <- df[(df$S003==32)&(df$S020==2006), ]

fit1 = Mclust(X$VOmoral, G=1)
summary(fit1)
plot(fit1, what = "density", data = X$VOmoral, breaks = 15)

fit2 = Mclust(X$VOmoral, G=2)
summary(fit2)
plot(fit2, what = "density", data = X$VOmoral, breaks = 15)

fit1$loglik
fit2$loglik

fit2$loglik - fit1$loglik #this should be positive in bimodality

fit2$bic-fit1$bic

# MCLUST PAÍSES
X.clust <- df[,c("S003", "S020", "VOmoral", "VOeco")]
ctry.nums <- c(32,76,170,484)
ctry.names <- c("Argentina", "Brazil", "Colombia", "Mexico")

bicdif.moral <- c()
bicdif.eco <- c()
bicdif.ctry <- c()
bicdif.yr <- c()
k=1
index=1
for (ctry in ctry.nums){
anios <- unique(X.clust[X.clust$S003==ctry,]$S020)
bicdif.ctry <- c(bicdif.ctry, rep(ctry.names[k], length(anios)))
for(yr in anios){
  fit1.moral = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral, G=1)
  fit1.eco = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco, G=1)
  fit2.moral = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral, G=2)
  fit2.eco = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco, G=2)
  bicdif.moral[index] <- fit2.moral$bic-fit1.moral$bic
  bicdif.eco[index] <- fit2.eco$bic-fit1.eco$bic
  bicdif.yr[index] <- yr
  index = index + 1
}
k=k+1
}

bicdifs <- cbind(bicdif.ctry, bicdif.yr, bicdif.moral, bicdif.eco)
bicdifs <- data.frame(bicdifs)

bicdifs$bicdif.moral.txt <- ifelse(bicdifs$bicdif.moral>0, "Si", "No")
bicdifs$bicdif.eco.txt <- ifelse(bicdifs$bicdif.eco>0, "Si", "No")

BD <- bicdifs[,c("bicdif.ctry", "bicdif.yr","bicdif.moral.txt","bicdif.eco.txt")]
stargazer(BD, covariate.labels = c("","País", "Año", "Moral", "Económica"),
          summary=F, align=T,out="BICDIF.htm")
#

# MCLUST PAÍSES  AVERAGED
X.clust <- df[,c("S003", "S020", "VOmoral", "VOeco")]
ctry.nums <- c(32,76,170,484)
ctry.names <- c("Argentina", "Brazil", "Colombia", "Mexico")

bicdif.moral <- c()
bicdif.eco <- c()
bicdif.ctry <- c()
bicdif.yr <- c()
k=1
index=1
for (ctry in ctry.nums){
  anios <- unique(X.clust[X.clust$S003==ctry,]$S020)
  bicdif.ctry <- c(bicdif.ctry, rep(ctry.names[k], length(anios)))
  for(yr in anios){
    f1.moral <- c()
    f1.eco <- c()
    f2.moral <- c()
    f2.eco <- c()
    for(m in 1:5){
    fit1.moral = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral, G=1)
    fit1.eco = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco, G=1)
    fit2.moral = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral, G=2)
    fit2.eco = Mclust(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco, G=2)
    f1.moral <- c(fit1.moral$bic)
    f1.eco <- c(fit1.eco$bic)
    f2.moral <- c(fit2.moral$bic)
    f2.eco <- c(fit2.eco$bic)
    }
    bicdif.moral[index] <- mean(f2.moral)-mean(f1.moral)
    bicdif.eco[index] <- mean(f2.eco)-mean(f1.eco)
    bicdif.yr[index] <- yr
    index = index + 1
  }
  k=k+1
}

bicdifs <- cbind(bicdif.ctry, bicdif.yr, bicdif.moral, bicdif.eco)
bicdifs <- data.frame(bicdifs)

bicdifs$bicdif.moral.txt <- ifelse(bicdifs$bicdif.moral>0, "Si", "No")
bicdifs$bicdif.eco.txt <- ifelse(bicdifs$bicdif.eco>0, "Si", "No")

BD <- bicdifs[,c("bicdif.ctry", "bicdif.yr","bicdif.moral.txt","bicdif.eco.txt")]
stargazer(BD, covariate.labels = c("","País", "Año", "Moral", "Económica"),
          summary=F, align=T,out="BICDIF_avg.htm")


# VARIABLES CORRELATION
X.clust <- df[,c("S003", "S020", "VOmoral", "VOeco")]
ctry.nums <- c(32,76,170,484)
ctry.names <- c("Argentina", "Brazil", "Colombia", "Mexico")

cor.ctry <- c()
cor.value <- c()
cor.yr <- c()
cor.pval <- c()

k=1
index=1
for (ctry in ctry.nums){
  anios <- unique(X.clust[X.clust$S003==ctry,]$S020)
  cor.ctry <- c(cor.ctry, rep(ctry.names[k], length(anios)))
  for(yr in anios){
    test <- cor.test(X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral, 
                     X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco)
    cor.value[index] <- test$estimate
    cor.pval[index] <- test$p.value
    cor.yr[index] <- yr
    index = index + 1
  }
  k=k+1
}

cors <- cbind(cor.ctry, cor.yr, round(cor.value,3), round(cor.pval,3))
cors <- data.frame(cors)
colnames(cors) <- c("Country", "Year", "Cor", "Pval")

cors$CorSignif <- ifelse(cors$Pval<0.05, "Si", "No")


stargazer(cors, column.sep.width = "20pt",
          summary=F, align=F, out="CorsOmnib.htm")

