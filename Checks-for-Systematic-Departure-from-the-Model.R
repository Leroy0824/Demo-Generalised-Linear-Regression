checking<-function(ymatrix,xmatrix,famil){
    model1<-glm(ymatrix~.,data=data.frame(ymatrix,xmatrix),family=famil)
    print(summary(model1))
    mu<-predict(model1,type="response")
    eta<-predict(model1,type="link")
    par(mfrow=c(1,2))
    # See Remarks 1-2
    plot(rstandard(model1),mu,
    xlab="Standardised deviance",ylab="Fitted linear predictor")
    plot(rstandard(model1),eta,
    xlab="Standardised deviance",ylab="Fitted mean")
    # See Remark 3
    if (famil=="poisson"){
        z<-log(mu)+(y-mu)/mu
    }
    if (famil=="gaussian"){
        z<-y
    }
    if (famil=="binomial"){
        z<-log(mu/(1-mu))+(y-mu)/(mu*(1-mu))
    }
    if (famil=="Gamma"){
        z<-1/mu-(y-mu)/(mu^2)
    }
    par(mfrow=c(3,dim(xmatrix)[2]/3))
    # See Remark 4
    for (i in 1:dim(xmatrix)[2]){
        temp4<-z-eta+coef(model1)[i]*x[,i]
        plot(temp4,x[,i],ylab=paste("covariate",i),xlab="ei")
        temp<-lm(x[,i]~temp4)
        delta<-(max(temp4)-min(temp4))/10
        x2<-seq(min(temp4)-delta,max(temp4)+delta,by=((max(temp4)-min(temp4))/1000))
        lines(x2,temp$coefficients[1]+temp$coefficients[2]*x2)
    }
    par(mfrow=c(1,1))
    cooks.distance(model1)
    # See Remark 5
    plot(cooks.distance(model1),
    xlab="ith observations",ylab="cook's distance")
    temp<-xmatrix%*%solve(t(xmatrix)%*%xmatrix)%*%t(xmatrix)
    temp<-as.matrix(temp)
    set1<-matrix(0,nrow=1,ncol=nrow(temp))
    for (i in 1:nrow(temp)){
        set1[i]<-temp[i,i]
    }
    as.array(set1)
    # See Remark 6
    plot(1:length(as.list(set1)),as.list(set1),
    xlab="ith observations",
    ylab="leverage")
}

library(faraway)
data(gala)
x<-as.matrix(gala[,2:dim(gala)[2]])
y<-as.matrix(gala[,1])
checking(y,x,"gaussian")