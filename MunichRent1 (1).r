#################################################
#
#
# MunichRent1.r
#
#
#################################################







#################################################
#
# 2.  Munich Rent per square meter  vs. Area
#
#      see FKLM example 3.3
#
#################################################




## read in data
Munich=read.csv("rent99.raw",sep=" ")
str(Munich)
head(Munich)

##
## fit linear model
##
lm.rent=lm(rent~area,data=Munich)
summary(lm.rent)

##
## plots
##

par(mfrow=c(1,3))
## plot data, response varialbe is stopping distance
plot(Munich$area,Munich$rent)
## make x-values for regression line
xvals=data.frame(area=seq(20,160,by=1))
## get regression line at xvals
yvals=predict(lm.rent,xvals)
## plot regression line
points(xvals$area,yvals,type="l",col="red",lwd=3)

## plot residuals 
res=resid(lm.rent)
plot(Munich$area,res)
abline(h=0,col="blue")

## QQ plot on residuals
qqnorm(res)
qqline(res,col="green")

## Residual Analysis:
##     (a) Strong heteroscedasticity!  Errors do NOT have equal variances
##     (b) Some outliers relative to a normal distribution (related to heteroscedasticity)             
##
##     Result: Linear model is not appropriate without adjustments
##     


##
## fit linear model using LOG of response
##
lm.rent2=lm(log(rent)~area,data=Munich)
summary(lm.rent2)

##
## plots
##

par(mfrow=c(1,3))
## plot data
plot(Munich$area,log(Munich$rent))
## make x-values for regression line
xvals=data.frame(area=seq(20,160,by=1))
## get regression line at xvals
yvals=predict(lm.rent2,xvals)
## plot regression line
points(xvals$area,yvals,type="l",col="red",lwd=3)

## plot residuals 
res=resid(lm.rent2)
plot(Munich$area,res)
abline(h=0,col="blue")

## QQ plot on residuals
qqnorm(res)
qqline(res,col="green")


## Residual Analysis:
##     (a) Heteroscedasticity is better (not perfect)
##     (b) Residuals are not normal.  (right skewed)             
##
##     Linear model is OK here, but residuals are not normal
##     


