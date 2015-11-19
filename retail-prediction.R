# Loading data ------------------------------------------------------------
rm(list=ls())
# install.packages("randomForest")
library(randomForest)
library(MASS)
library(lmtest)
path=paste("C:\\Users\\Jan\\OneDrive\\MSiA\\",
           "MSiA401 - Predictive Analytics -Ajit Tamhane\\",
           "retail-prediction", sep="")
setwd(path)
train=read.csv("train.csv")
test=read.csv("test.csv")
tr=train  # Keep original data for reference for
ts=test   # train and test set. Work with tr&ts.
attach(tr)

# Transforming and adding variables ---------------------------------------
### targamnt
tr$targamnt=ifelse(targamnt==0,0,1) # Adding BOOL 1/0 (Sale/NoSale)
# When modelling sales: double log!
# hist(log(log(tr[!targamnt==0,"targamnt"])))

### ordcls 1-7
# The following condenses this predictor into 2-3 groups for all seven
# categories, based on their distribution.
ordclsFREQ=matrix(, nrow=7,ncol=11)
for (i in 1:11){ordclsFREQ[1,i]=nrow(tr[tr$ordcls1==i-1,])}
for (i in 1:11){ordclsFREQ[2,i]=nrow(tr[tr$ordcls2==i-1,])}
for (i in 1:11){ordclsFREQ[3,i]=nrow(tr[tr$ordcls3==i-1,])}
for (i in 1:11){ordclsFREQ[4,i]=nrow(tr[tr$ordcls4==i-1,])}
for (i in 1:11){ordclsFREQ[5,i]=nrow(tr[tr$ordcls5==i-1,])}
for (i in 1:11){ordclsFREQ[6,i]=nrow(tr[tr$ordcls6==i-1,])}
for (i in 1:11){ordclsFREQ[7,i]=nrow(tr[tr$ordcls7==i-1,])}
ordclsFREQ # This frequency table helps group the order predictors.
tr$ordcls1=ifelse(ordcls1>=1,1,0)
tr$ordcls2=ifelse(tr$ordcls2>=2,2,ifelse(tr$ordcls2==1,1,0))
tr$ordcls3=ifelse(tr$ordcls3>=2,2,ifelse(tr$ordcls2==1,1,0))
tr$ordcls4=ifelse(ordcls4>=1,1,0)
tr$ordcls5=ifelse(tr$ordcls5>=2,2,ifelse(tr$ordcls5==1,1,0))
tr$ordcls6=ifelse(tr$ordcls6>=2,2,ifelse(tr$ordcls6==1,1,0))
tr$ordcls7=ifelse(tr$ordcls7>=2,2,ifelse(tr$ordcls7==1,1,0))

### Salcls 1-7
# plot(salcls1)
# plot(salcls2)
# plot(salcls3)
# plot(salcls4)
# plot(salcls5)
# plot(salcls6)
# plot(salcls7)
# TODO: Remove outliers!

# When looking at histograms, it turns out that taking the log
# makes these predictors approx. normal distributed:
hist(tr[!salcls1==0,"salcls1"])
hist(log(tr[!salcls1==0,"salcls1"]))
# Looks roughly the same for all seven categories.
# Therefore, log is applied to all seven.
# Use salcls1<0.001 instead of salcls1==0.001, because of some
# weird rounding errors or something else producing NaN's.
tr[!salcls1<0.001,"salcls1"]=log(tr[!salcls1<0.001,"salcls1"])
tr[!salcls2<0.001,"salcls2"]=log(tr[!salcls2<0.001,"salcls2"])
tr[!salcls3<0.001,"salcls3"]=log(tr[!salcls3<0.001,"salcls3"])
tr[!salcls4<0.001,"salcls4"]=log(tr[!salcls4<0.001,"salcls4"])
tr[!salcls5<0.001,"salcls5"]=log(tr[!salcls5<0.001,"salcls5"])
tr[!salcls6<0.001,"salcls6"]=log(tr[!salcls6<0.001,"salcls6"])
tr[!salcls7<0.001,"salcls7"]=log(tr[!salcls7<0.001,"salcls7"])

### ord185-ord485: Use as is.

### tof: Use log.
hist(tof)
hist(log(tof))
tr$tof=log(tr$tof)

### totord
# Also heavily right-skewed.
hist(log(log(log(tr[totord>3,"totord"]))))
hist(tr[totord>0,"totord"])
hist(tr[totord>1,"totord"])
hist(tr[totord>2,"totord"])
hist(tr[totord>5,"totord"])
# Hard to tell what to do with it.
# Maybe group into one-time/two-time customers and regular buyers:
tr$totord=ifelse(tr$totord>2,1,0)

### totsale
hist(log(tr$totsale))
# Make log transformation again.
tr$totsale=log(tr$totsale+1)
tr[!totsale<=0.1,"totsale"]=log(tr[!totsale<0.1,"totsale"])
tr[,"totsale"]

# Logistic regression -----------------------------------------------------

fitLog=glm(targamnt~recmon+ordcls1+ordcls2+ordcls3+ordcls4+ordcls5+ordcls6+
           ordcls7+salcls1+salcls2+salcls3+salcls4+salcls5+salcls6+salcls7+
           ord185+ord285+ord385+ord485+tof+totord+totsale
           ,family=binomial,data=tr)

fitLogStep=stepAIC(object=fitLog, direction="both")  
summary(fitLogStep)
fitNull=glm(targamnt~1,family=binomial,data=tr)
lrtest(fitLogStep,fitNull) # Model is not totally non-significant!!

# Random forest -----------------------------------------------------------

fitRF=randomForest(
  targamnt~recmon+ordcls1+ordcls2+ordcls3+ordcls4+ordcls5+ordcls6+
  ordcls7+salcls1+salcls2+salcls3+salcls4+salcls5+salcls6+salcls7+
  ord185+ord285+ord385+ord485+tof+totord+totsale,
  data=tr,ntree=100,mtry=5,importance=TRUE,na.action=na.omit)
fitRF
importance(fitRF)
varImpPlot(fitRF)
# names(fitRF)

names(fitLogStep$coefficients)

fitRF2=randomForest(
  targamnt~recmon+ordcls2+ordcls3+ordcls7+salcls3+salcls4+salcls6+
    ord185+ord285+ord385+ord485+tof+totord+totsale,
  data=tr,ntree=100,mtry=5,importance=TRUE,na.action=na.omit)
fitRF2
importance(fitRF2)
varImpPlot(fitRF2)

# Clean up ----------------------------------------------------------------
detach(tr)