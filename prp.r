prp <- function(fit,data,names=names(data),max.num.cat=10,add.smooth=TRUE,output=FALSE,...){

    ##
    ## prp.r - Partial Residual Plots in R
    ##
    ##    Author: Ephraim M. Hanks
    ##            hanks@psu.edu
    ##
    ##    Last Update: 20150915
    ##
    ## Inputs
    ##    fit = an "lm" object
    ##    data = the dataframe used in "fit"
    ##    names = a vector of the column names in "data" for which to
    ##            make a partial residual plot
    ##    max.num.cat = the maximum number of unique values of a covariate
    ##            for prp to define it as a categorical covariate.  Any
    ##            covariate with more than this number of unique values
    ##            will be treated as a continuous covariate.
    ##    add.smooth = Logical.  If TRUE, add a trend line estimated
    ##            using Loess smoothing
    ##    output = Logical.  If TRUE, output the following:
    ##
    ## Outputs
    ##    A list with one list element per entry in "names", each with
    ##    the following elements
    ##
    ##    name = The column name from "data"
    ##    covariate = the data column from "data"
    ##    part.resid = the k-th partial residual
    ##    fhat = the estimated regression line for the k-th partial residual
    ##
    ##
    
    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    
    p=length(names)
    eps=resid(fit)

    means=apply(data,2,mean)
    
    if(p<4){
        par(mfrow=c(1,p))
    }
    if(p>3 & p<9){
        par(mfrow=c(2,round(p/2)))
    }
    if(p>8){
        par(mfrow=c(3,floor(p/3)+1))
    }

    idx.cat=integer()
    meanData=data
    means=apply(data,2,mean)
    for(i in 1:ncol(data)){
        meanData[,i]=mean(meanData[,i])
        if(length(unique(data[,i]))<max.num.cat){
            meanData[,i]=Mode(data[,i])
        }
    }

    outlist=list()
    
    for(k in 1:p){
        newData=meanData
        idx=which(names(data)==names[k])
        newData[,idx]=data[,idx]
        eps.k=predict(fit,newdata=newData)+eps
        plot(newData[,idx],eps.k,xlab=names[k],ylab="Partial Residual",main=names[k],...)
        sort.idx=sort(newData[,idx],index.return=T)$ix
        newData=newData[sort.idx,]
        regline=predict(fit,newdata=newData)
        outlist[[k]]=list(name=names[k],covariate=newData[,idx],part.resid=eps.k,fhat=regline)
        if(add.smooth){
            ll=suppressWarnings(loess(eps.k[sort.idx]~newData[,idx]))
            points(ll$x,ll$fitted,type="l",lwd=5,lty=2,col="blue")
        }
        points(newData[,idx],regline,type="l",lwd=5,col="red")
    }
    if(output){
        outlist
    }
}
