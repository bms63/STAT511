#################################################
##
##
## MunichInteractions.r
##
##
#################################################


## read in rent99.raw data
Munich=read.csv("rent99.raw",sep=" ")

str(Munich)       ## gives general structure
summary(Munich)   ## computes summary statistics of each column
head(Munich)      ## shows the first 6 rows of the data frame

fit=lm(rent~area,data=Munich)
summary(fit)

yhat=fit$fitted
res=fit$res

##
## Diagnostic plots
##

## plot fitted values vs residuals (check for heteroscedasticity)
plot(yhat,res)
## plot residuals vs each covariate (check for heteroscedasticity and nonlinearity)
plot(Munich$area,res)
abline(0,0,col="red")
## partial residual plots (requires the "car" package)
library(car)
crPlots(fit)
## QQ-plot of residuals (check normality)
qqnorm(res)
qqline(res)




##
## adding in a categorical covariate
##

plot(yhat,res,col=Munich$kitchen+1,pch=20,cex=3)

fit=lm(rent~area+kitchen,data=Munich)
summary(fit)



##
## Plotting mean function by predicting mean
##

plot(Munich$area,Munich$rent,col=Munich$kitchen+1,pch=20,cex=1)

## create a data frame with kitchen=1
area=seq(20,160)
kitchen.df=data.frame(area,kitchen=1)
head(kitchen.df)

## create a data frame with kitchen=0
no.kitchen.df=data.frame(area,kitchen=0)
head(no.kitchen.df)

## get E[area|kitchen=1] using "predict"
kitchen.mean=predict(fit,newdata=kitchen.df)
points(area,kitchen.mean,col="red",type="l",lwd=5)

## get E[area|kitchen=0] using "predict"
no.kitchen.mean=predict(fit,newdata=no.kitchen.df)
points(area,no.kitchen.mean,col="black",type="l",lwd=5)


##
## Plotting mean function using "abline"
##

betahat=coef(fit)
betahat

plot(Munich$area,Munich$rent,col=Munich$kitchen+1,pch=20,cex=1)
abline(betahat[1]+betahat[3],betahat[2],col="red",lwd=5)
abline(betahat[1],betahat[2],col="black",lwd=5)



##########################################################
##
## adding in an interaction term
##
##########################################################

fit=lm(rent~area*factor(kitchen),data=Munich)
summary(fit)

beta.hat=fit$coeff
beta.hat
##
## plotting interaction effect
##

plot(Munich$area,Munich$rent,pch=20,col=Munich$kitchen+1)
abline(beta.hat[1],beta.hat[2],lwd=5)
abline(beta.hat[1]+beta.hat[3],beta.hat[2]+beta.hat[4],lwd=5,col=2)





##
## Now an interaction between two categorical covariates
##

## categorical dummy coding by hand for "location" 3=best, 2=mid-range location, 1=worst

Munich$loc2=rep(0,nrow(Munich))
Munich$loc2[Munich$location==2]=1

Munich$loc3=rep(0,nrow(Munich))
Munich$loc3[Munich$location==3]=1

fit=lm(rent~area+loc2*kitchen+loc3*kitchen,data=Munich)

summary(fit)

## The same model fit using the "factor" statement in LM

fit=lm(rent~area+factor(location)*kitchen,data=Munich)
summary(fit)


## getting regression parameter estimates
beta.hat=fit$coeff
beta.hat

## plotting regression lines:
## categories:
## 1. loc=1, kitchen=0
## 2. loc=1, kitchen=1
## 3. loc=2, kitchen=0
## 4. loc=2, kitchen=1
## 5. loc=3, kitchen=0
## 6. loc=3, kitchen=1

plot.color=rep(NA,nrow(Munich))
plot.color[Munich$location==1 & Munich$kitchen==0] <- 1
plot.color[Munich$location==1 & Munich$kitchen==1] <- 2
plot.color[Munich$location==2 & Munich$kitchen==0] <- 3
plot.color[Munich$location==2 & Munich$kitchen==1] <- 4
plot.color[Munich$location==3 & Munich$kitchen==0] <- 5
plot.color[Munich$location==3 & Munich$kitchen==1] <- 6

plot(Munich$area,Munich$rent,pch=20,col=plot.color)
abline(beta.hat[1],beta.hat[2],col=1)
abline(beta.hat[1]+beta.hat[5],beta.hat[2],col=2)
abline(beta.hat[1]+beta.hat[3],beta.hat[2],col=3)
abline(beta.hat[1]+beta.hat[3]+beta.hat[5]+beta.hat[6],beta.hat[2],col=4)
abline(beta.hat[1]+beta.hat[4],beta.hat[2],col=5)
abline(beta.hat[1]+beta.hat[4]+beta.hat[5]+beta.hat[7],beta.hat[2],col=6)
## too many categories to get much from this plot!


