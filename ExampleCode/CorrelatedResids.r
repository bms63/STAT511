##
## isj
##


load("isj.Rdata")
head(isj)

plot(isj[,2:3],type="n",main="Elevation",asp=1)
na.idx=which(is.na(isj$elev))
elev.star=isj$elev[-na.idx]
norm.elev=(elev.star-min(elev.star))/(max(elev.star)-min(elev.star))
points(isj[-na.idx,2:3],pch=20,col=grey(1-norm.elev))
points(isj[1:307,2:3],pch=4,col="red")

occ=isj[-which(is.na(isj$isj)),]
occ=occ[-which(is.na(occ$elev)),]

fit=glm(isj~poly(elev,1)+poly(forest,2)+poly(chap,2),family=binomial,data=occ)
summary(fit)

resids=residuals(fit)
plot(resids)
plot(isj[,2:3],type="n",main="Elevation",asp=1)


isj.nona=na.omit(isj[,-1])
str(isj.nona)
phat=predict(fit,newdata=isj.nona,type="response")

plot(isj[,2:3],type="n",main="Plotting Residuals in Space",asp=1)
points(isj.nona[,1:2],pch=20,col=grey(1-phat))
points(isj[1:307,2:3],pch=1,col="red",cex=(resids-min(resids))/(max(resids)-min(resids))*2)


##
## Nile
##

plot(Nile,main="Annual flow of the river Nile at Ashwan")


## date when Ashwan dam was constructed

abline(v=1902,col="blue")

## create covariate
y=as.numeric(Nile)
x=rep(0,length(y))
1902-1871
x[32:length(x)]=1

plot(x)
pairs(cbind(y,x))

## fit linear model

fit=lm(y~x)
summary(fit)

ypred=predict(fit)
res=resid(fit)

## examine model fit graphically
plot(Nile,main="Annual flow of the river Nile at Ashwan - Data with Fitted Model")
points(1871:1970,ypred,type="l",col="red")

## examine residuals for normality

par(mfrow=c(1,2))
hist(res,main="Histogram of Model Residuals")
qqnorm(res)
qqline(res,col="red")


par(mfrow=c(1,1))
pncol=rep(2,length(res))
pncol[res<0] <- 4
plot(1871:1970,res,pch=20,type="l",main="Model Residuals Over Time",col=1)
points(1871:1970,res,pch=20,type="p",col=pncol,cex=3)
abline(h=0)


## examine residuals for serial correlation

r=res[-1]
r.prev=res[-length(res)]

plot(r,r.prev)
abline(lm(r~r.prev))
summary(lm(r~r.prev))

acf(res)


## Specify and fit a Linear Model with AR(1) time series correlated random effect

library(nlme)

group=rep(1,length(y))
t=1871:1970

fit.gls=gls(y~x,correlation=corAR1(form= ~t))
summary(fit.gls)
intervals(fit.gls)

