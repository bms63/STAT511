################################################
##
## numericMLE.r
##
## example of numerical MLEs and standard errors
##

################################################

## Use of "optim" in R
##
## 1. Make a function to evaluate the negative log-likelihood
##    This function must have as it's first argument a vector of all model parameters
##    It may have other arguments (like the data!)
##
## 2. Find the MLEs using
##      optim(start.values,nll.function,other arguments...)


##########################################
## Illustration of Newton's method in 1-d
##########################################

install.packages("animation")

library(animation)

xx=newton.method()
newton.method(function(x) 5 * x^3 - 7 * x^2 - 40 * x + 100, 7.15, c(-6.2,7.1))


##########################################
## Numerical MLEs for regression
##########################################

## write a function that takes parameters
## (in beta.s2) and returns the negative
## log-likelihood


nll.reg <- function(beta.s2,y,X){
    ## get parameters
    p=length(beta.s2)-1
    n=length(y)
    beta=beta.s2[1:p]
    s2=beta.s2[p+1]
    ## calculate loglikelihood
    loglik=sum(dnorm(y,X%*%beta,sqrt(s2),log=T))
    ## return negative loglikelihood
    -loglik
}

##
## read in some data and estimate parameters using "lm"
##


summary(lm(dist~speed, data=cars))
cars
y=cars$dist
X=cbind(1,cars$speed)

fit=lm(y~0+X)
summary(fit)

##
## find MLE using "optim" numerical optimization
##

beta.start=c(0,0)
s2.start=1
out=optim(c(beta.start,s2.start),nll.reg,y=y,X=X,control=list(trace=10),hessian=T)

## get parameter estimates (order is the same as in beta.s2)
out$par

## compare to estimates from lm:
coef(fit)

## get standard errors from optim
H=out$hessian
S=solve(H)
se=sqrt(diag(S))
se

summary(lm(dist~speed, data=cars))


##########################################
## Numerical MLEs for logistic regression
##########################################

## read in data
admissions <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(admissions)

y=admissions$admit
X=as.matrix(cbind(1,admissions[,2:4]))

head(X)
## fit logistic regression model
fit=glm(y~0+X,family="binomial")
summary(fit)


##
## write function to evaluate log-likelihood
##
## f(y_i | beta) = (p_i)^y_i * (1-p_i)^(1-y_i)
##  =>
## nll(y_i) = -[y_i log(p_i) + (1-y_i) log(1-p_i)]

nll.logistic.reg <- function(beta,y,X){
    ## linear predictor
    eta=X%*%beta
    ## apply response function
    p=exp(eta)/(1+exp(eta))
    ## evaluate negative log likelihood
    nll=-sum(y*log(p)+(1-y)*log(1-p))
}

beta.start=c(0,0,0,0)
out=optim(beta.start,nll.logistic.reg,y=y,X=X,control=list(trace=10),hessian=T)

## get parameter estimates
out$par
## compare to MLEs from glm
fit$coef


## get standard errors
H=out$hessian
S=solve(H)
se=sqrt(diag(S))
se

## compare to se's from glm
summary(fit)$coef[,2]


