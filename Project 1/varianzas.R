library(ggplot2)
#install.packages("dplyr")
library(dplyr)

X.yrs <- df[df$S003==32, c("S020", "VOmoral","VOeco")]

ggplot(X.yrs, aes(factor(S020), VOmoral))+
  stat_boxplot( aes(factor(S020), VOmoral), 
                geom='errorbar', linetype=1, width=0.3)+  #whiskers
  geom_boxplot( aes(factor(S020), VOmoral),outlier.shape=1) +    
  stat_summary(fun=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")

ggplot(X.yrs, aes(factor(S020), VOeco))+
  stat_boxplot( aes(factor(S020), VOeco), 
                geom='errorbar', linetype=1, width=0.3)+  #whiskers
  geom_boxplot( aes(factor(S020), VOeco),outlier.shape=1) +    
  stat_summary(fun=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")

s <- aggregate(X.yrs[, c("VOmoral")], list(S020=X.yrs$S020), sd)
yrs <- s[1]
s <- s[2]
mn <- aggregate(X.yrs[, c("VOmoral")], list(S020=X.yrs$S020), mean)[2]
ci1 <- mn - qnorm(0.95)*s
ci2 <- mn + qnorm(0.95)*s
minm <- aggregate(X.yrs[, c("VOmoral")], list(S020=X.yrs$S020), min)[2]
maxm <- aggregate(X.yrs[, c("VOmoral")], list(S020=X.yrs$S020), max)[2]
ln <- aggregate(X.yrs[, c("VOmoral")], list(S020=X.yrs$S020), length)[2]
ref.dat <- cbind(yrs,s,mn,ci1,ci2,minm,maxm, ln)
colnames(ref.dat) <- c("S020", "s", "m", "ci1", "ci2", "min", "max", "N")
ref.mean <- cbind(yrs,mn)

X.moral <- df[df$S003==32, c("S020", "VOmoral")]
X.moral <- X.moral %>% group_by(S020) %>% mutate(s = sd(VOmoral))
X.moral <- X.moral %>% group_by(S020) %>% mutate(m = mean(VOmoral))

ref.dat <- data.frame(ref.dat)
ref.dat$S020 <- haven::as_factor(ref.dat$S020)

ggplot(data=ref.dat, aes(x=S020, y=m, group=1)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(y = m, ymin = m-s, ymax = m+s), alpha = .2)+
  theme_bw()

X.moral$S020 <- haven::as_factor(X.moral$S020)
ggplot(data=X.moral, aes(x=S020, y=m, group=1)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(y = m, ymin = m-s, ymax = m+s), alpha = .2)+
  theme_bw()

### Varianzas de varios países

X.var.todos <- df[, c("S020","S003", "VOmoral", "VOeco")]
X.var.todos <- X.var.todos %>% group_by(S003, S020) %>% mutate(s.moral = sd(VOmoral))
X.var.todos <- X.var.todos %>% group_by(S003, S020) %>% mutate(m.moral = mean(VOmoral))
X.var.todos <- X.var.todos %>% group_by(S003, S020) %>% mutate(s.eco = sd(VOeco))
X.var.todos <- X.var.todos %>% group_by(S003, S020) %>% mutate(m.eco = mean(VOeco))
X.var.todos <- X.var.todos %>% group_by(S003, S020) %>% mutate(N = length(VOmoral))
X.var.todos <- X.var.todos %>% 
  group_by(S003, S020) %>% 
  mutate(ci.moral = qnorm(0.975)*s.moral/sqrt(N))
X.var.todos <- X.var.todos %>% 
  group_by(S003, S020) %>% 
  mutate(ci.eco = qnorm(0.975)*s.eco/sqrt(N))
X.var.todos$S020 <- haven::as_factor(X.var.todos$S020)
X.var.todos$S003 <- haven::as_factor(X.var.todos$S003)

ggplot(data=X.var.todos, aes(x=S020, y=m.moral, group=S003)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(y = m.moral, ymin = m.moral-s.moral,
                  ymax = m.moral+s.moral), alpha = .2)+
  theme_bw()

# SIN ERRORBAR 

ggplot(data=X.var.todos, aes(x=S020, y=m.moral, group=1)) +
  geom_line()+
  geom_point(size=2)+
  geom_ribbon(aes(y = m.moral, ymin = m.moral-s.moral,
                  ymax = m.moral+s.moral), alpha = .2)+
  xlab("Años")+
  ylab("Variable ómnibus moral")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill=NA, colour='white'),
        panel.grid.major = element_line(colour = "gray94"))+
  facet_wrap(~ S003)

ggplot(data=X.var.todos, aes(x=S020, y=m.eco, group=1)) +
  geom_line()+
  geom_point(size=2)+
  geom_ribbon(aes(y = m.eco, ymin = m.eco-s.eco,
                  ymax = m.eco+s.eco), alpha = .2)+
  xlab("Años")+
  ylab("Variable ómnibus Económica")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill=NA, colour='white'),
        panel.grid.major = element_line(colour = "gray94"))+
  facet_wrap(~ S003)


# CON ERRORBAR

ggplot(data=X.var.todos, aes(x=S020, y=m.moral, group=1)) +
  geom_line()+
  geom_point(size=1)+
  geom_ribbon(aes(y = m.moral, ymin = m.moral-s.moral,
                  ymax = m.moral+s.moral), alpha = .2)+
  geom_errorbar(data=X.var.todos, 
                mapping=aes(x=S020, ymin=m.moral-ci.moral, 
                            ymax=m.moral+ci.moral), 
                width=0.01, size=1, color="blue") + 
  xlab("Años")+
  ylab("Variable ómnibus moral")+
  coord_cartesian(ylim = c(100, 450)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill=NA, colour='white'),
        panel.grid.major = element_line(colour = "gray94"))+
  facet_wrap(~ S003)

ggplot(data=X.var.todos, aes(x=S020, y=m.eco, group=1)) +
  geom_line()+
  geom_point(size=1)+
  geom_ribbon(aes(y = m.eco, ymin = m.eco-s.eco,
                  ymax = m.eco+s.eco), alpha = .2)+
  geom_errorbar(data=X.var.todos, 
                mapping=aes(x=S020, ymin=m.eco-ci.eco, 
                            ymax=m.eco+ci.eco), 
                width=0.01, size=1, color="blue") + 
  xlab("Años")+
  ylab("Variable ómnibus Económica")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_rect(fill=NA, colour='white'),
        panel.grid.major = element_line(colour = "gray94"))+
  facet_wrap(~ S003)


# CUADRO VARIANZAS

X.clust <- df[,c("S003", "S020", "VOmoral", "VOeco")]
ctry.nums <- c(32,76,170,484)
ctry.names <- c("Argentina", "Brazil", "Colombia", "Mexico")

var.moral <- c()
var.eco <- c()
countries <- c()
YR.yr <- c()
k=1
index=1
for (ctry in ctry.nums){
  anios <- unique(X.clust[X.clust$S003==ctry,]$S020)
  countries <- c(countries, rep(ctry.names[k], length(anios)))
  for(yr in anios){
    x <- X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOeco
    var.eco[index] <- var(x)
    
    x <- X.clust[(X.clust$S003==ctry)&(X.clust$S020==yr),]$VOmoral
    var.moral[index] <- var(x)

    YR.yr[index] <- yr
    index = index + 1
  }
  k=k+1
}

bicdifs <- cbind(countries, YR.yr,round(var.moral,1),round(var.eco,1))
bicdifs <- data.frame(bicdifs)

library(stargazer)

stargazer(bicdifs, covariate.labels = c("","País", "Año", "Var Moral", "Var Económica"),
          summary=F, align=T,out="Varianzas_Tabla.htm")