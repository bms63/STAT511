
## Montana Data Description:

##      Dataset from a cohort study of exposure to arsenic from industry
##      and deaths from respiratory diseases.

##      A data frame with 114 observations on the following 6 variables.

##      ‘respdeath’ a numeric vector indicating number of deaths from
##           respiratory diseases

##      ‘personyrs’ a numeric vector indicating person-years of exposure

##      ‘agegr’ a numeric vector: 1=40-49, 2=50-59, 3=60-69, 4=70-79)

##      ‘period’ a numeric vector: 1=1938-1949, 2=1950-1959, 3=1960-1969,
##           4=1970-1977

##      ‘start’ a numeric vector indicating starting period: 1=pre-1925,
##           2=1925 & after

##      ‘arsenic’ a numeric vector indicating years of exposure: 1=<1
##           year, 2=1-4 years, 3=5-14 years, 4=15+ years



##
##
## Read in data
##
##

load("Montana.Rdata")
head(Montana)
pairs(Montana)

##
##
## Specify model with offset = log(personyrs)
##
##

fit=glm(respdeath~factor(agegr)+period+factor(arsenic)+start,offset=log(personyrs),family="poisson",data=Montana)
summary(fit)






###############################################
##
## Residual diagnostics
##
###############################################

## residual plots
plot(fit)

## partial residual plots

library(car)
crPlots(fit)

## Test for outliers
library(car)
outlierTest(fit)



## adding in a quadratic for "period"
fit=glm(respdeath~factor(agegr)+poly(period,2)+factor(arsenic)+start,offset=log(personyrs),family="poisson",data=Montana)
summary(fit)

## making "period" a factor
fit=glm(respdeath~factor(agegr)+factor(period)+factor(arsenic)+start,offset=log(personyrs),family="poisson",data=Montana)
summary(fit)





##
## simulation study to see if resids are acceptable
##

## simulate data from the fitted model
muhat=predict(fit,type="response")

ysim=rpois(length(muhat),lambda=muhat)

## fit the simulated data
fit.sim=glm(ysim~factor(agegr)+factor(period)+factor(arsenic)+start,offset=log(personyrs),family="poisson",data=Montana)
summary(fit.sim)

## residual plots
plot(fit.sim)

## These look qualitatively very similar to resids from our model fit
## Conclusion: No particular problems with residuals.  Model is OK.
##


###############################################
##
## Predictions 
##
###############################################

## predicted mean # of resp.deaths per year for 1000 people in a particular group
##  by setting "arsenic=1:4" I am jointly calculating everything for all 4 arsenic groups
##  This allows us to compare predicted respitory death rates across arsenic exposure levels

## make a data frame with the desired predictor variables
data=data.frame(personyrs=1000,agegr=2,period=1,arsenic=1:4,start=1)
data

## get the mean and sd of the linear predictor eta for those predictor variables
pred.mean=predict(fit,newdata=data,type="response",se=T)
mu.hat=pred.mean$fit

## CI bounds on linear predictor
CI.up=mu.hat+1.96*pred.mean$se.fit
CI.down=mu.hat-1.96*pred.mean$se.fit

## CI on LINEAR PREDICTOR in table form
cbind(CI.down,CI.up)

## CI on MEAN # of resp.deaqths per year for 1000 people
## Note: for Poisson regression, response function is mu=exp(eta)
exp(cbind(CI.down,CI.up))

