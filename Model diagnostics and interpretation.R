# Example 1
x<-c(1,2)
y<-c(0,1)
model1<-glm(y~ x-1,family=poisson(link="log"))
summary(model1)
coefficient<-summary(model1)$coefficients
temp<-exp(coefficient[1,1]*x) #Fitted value of mu
temp
summary(model1)$deviance #Deviance residual
residuals(model1,type="pearson")
rstandard(model1,type="pearson")

lmodel<-logLik(model1)
lnull<-logLik(update(model1,.~1))
rsquared<-1-(lmodel/lnull)
rsquared

# Example 2
library(faraway)
data(gala)
str(gala)
temp<-gala[,-2]
str(temp)
model1<-glm(Species~.,family=poisson(link="log"),data=gala)
model2<-summary(model1)
names(model2)
model2$deviance

set1<-c("Elevation","Nearest","Scruz","Adjacent","1")
for (i in set1){
    nullmodel<-update(model1,as.formula(paste(". ~ . -", i)))
    print(anova(nullmodel,model1,test="Chisq"))
}
fullmodel<-logLik(model1)
nullmodel<-logLik(update(model1,.~1))
1-fullmodel/nullmodel

par(mfrow=c(2,2))
plot(predict(model1,type="response"),residuals(model1,type="deviance"))
plot(predict(model1,type="link"),residuals(model1,type="deviance"))
plot(predict(model1,type="response"),residuals(model1,type="response"))
plot(predict(model1,type="link"),residuals(model1,type="response"))

mu<-fitted(model1,type="response")
res<-residuals(model1,type="response")
par(mfrow=c(2,2))
plot(mu,res^2,xlab="residuals",ylab="fitted mean^2")
abline(a=0,b=1)
plot(mu,res^2,xlab="residuals",ylab="fitted mean^2",xlim=c(0,100))
abline(a=0,b=1)
plot(mu,res^2,xlab="residuals",ylab="fitted mean^2",xlim=c(0,50))
abline(a=0,b=1)
plot(mu,res^2,xlab="residuals",ylab="fitted mean^2",xlim=c(15,35))
abline(a=0,b=1)

y<-gala$Species
z<-log(mu)+(y-mu)/mu
plot(z,log(mu),xlab="Adjusted dependent variable",ylab="linear predictor")
temp<-lm(log(mu)~z)
temp
x<-seq(1,8,by=0.001)
lines(x,temp$coefficients[1]+temp$coefficients[2]*x)

x<-model.matrix(model1)
eta<-predict(model1,type="link")
par(mfrow=c(3,2))
set1<-c("Endemics","Area","Elevation","Nearest","Scruz","Adjacent")
for (i in set1){
temp4<-z-eta+coef(model1)[i]*x[,i]
    plot(temp4,x[,i],ylab=paste("covariate",i),xlab="ei")
    temp<-lm(x[,i]~temp4)
    x2<-seq(-10,10,by=0.001)
    lines(x2,temp$coefficients[1]+temp$coefficients[2]*x2)
}

Xmatrix<-gala[,2:dim(gala)[2]]
Ymatrix<-gala[,1,drop=FALSE]
colnames(Ymatrix)<-"Species"
par(mfrow=c(3,3))
temp<-cbind(Ymatrix,Xmatrix)
for (i in 1:dim(temp)[2]){
    halfnorm(temp[,i],ylab=colnames(temp[i]))

}
temp<-cooks.distance(model1)
par(mfrow=c(1,1))
plot(temp)
temp