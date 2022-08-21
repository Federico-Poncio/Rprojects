# Modelos y assumptions
install.packages("gvlma")
X.reg <- dataset[,c("S020","S003","X003", 
                    "Ideolog","e.ImpRelig_lin","X025R",
                    "VOmoral","VOeco")]
X.reg <- na.omit(X.reg)
X.reg.arg <- X.reg[X.reg$S003==32,]

lm.1995 <- lm(VOmoral ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
              data=X.reg.arg[X.reg.arg$S020==1995,])
summary(lm.1995)
par(mfrow = c(2, 2))
plot(lm.1995)

hist(lm.1995$residuals)

gvlma::gvlma(lm.1995)

lm.1995.ln <- lm(log(VOmoral+1) ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
              data=X.reg.arg[X.reg.arg$S020==1995,])
lm.1995.sq <- lm(sqrt(VOmoral) ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
              data=X.reg.arg[X.reg.arg$S020==1995,])

summary(lm.1995.sq)
par(mfrow = c(2, 2))
plot(lm.1995.sq)

lm.1999 <- lm(VOmoral ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
              data=X.reg.arg[X.reg.arg$S020==1999,])
summary(lm.1999)
par(mfrow = c(2, 2))
plot(lm.1999)
gvlma::gvlma(lm.1999)
hist(lm.1999$residuals)

ctry = 76
yr = 2014

LM <- lm(VOmoral ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
              data=X.reg[(X.reg$S003==ctry)&(X.reg$S020==yr),])
summary(LM)
par(mfrow = c(2, 2))
plot(LM)
gvlma::gvlma(LM)
hist(LM$residuals)

anios <- unique(X.reg[X.reg$S003==32,]$S020)
models <- c()
index=1
for(yr in anios){
  lm.anio <- lm(VOmoral ~ X003 + factor(Ideolog) + factor(e.ImpRelig_lin) +factor(X025R), 
                data=X.reg.arg[X.reg.arg$S020==yr,])
  summary(lm.anio)
  par(mfrow = c(2, 2))
  plot(lm.anio)
  print(gvlma::gvlma(lm.anio))
  hist(lm.anio$residuals)
  index = index + 1
}

plot.new()
anios <- unique(X.reg[X.reg$S003==32,]$S020)
models <- c()
index=1
for(yr in anios){
  lm.anio <- lm(VOmoral ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
                data=X.reg.arg[X.reg.arg$S020==yr,])
  summary(lm.anio)
  par(mfrow = c(2, 2))
  plot(lm.anio, main=paste0("Argentina.",yr))
  print(paste0("###### Argentina.",yr))
  print(gvlma::gvlma(lm.anio))
  par(mfrow = c(1,1))
  hist(lm.anio$residuals, main= paste0("Residuos Argentina.",yr))
  index = index + 1
}



plot.new()
anios <- unique(X.reg[X.reg$S003==76,]$S020)
models <- c()
index=1
for(yr in anios){
  lm.anio <- lm(VOmoral ~ X003 + Ideolog + e.ImpRelig_lin +X025R, 
                data=X.reg[(X.reg$S003==76)&(X.reg$S020==yr),])
  summary(lm.anio)
  par(mfrow = c(2, 2))
  plot(lm.anio, main=paste0("Brasi.",yr))
  print(paste0("###### Brasi.",yr))
  print(gvlma::gvlma(lm.anio))
  par(mfrow = c(1,1))
  hist(lm.anio$residuals, main= paste0("Residuos Brasi.",yr))
  index = index + 1
}