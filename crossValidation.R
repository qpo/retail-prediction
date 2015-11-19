# Loading data ------------------------------------------------------------
rm(list=ls())
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
plot(salcls1)
hist(tr[!salcls1==0,"salcls1"])
hist(log(tr[!salcls1==0,"salcls1"]))


# Logistic regression -----------------------------------------------------

fitLog=glm(targamnt~totsale,family=binomial,data=tr)
fitNull=glm(targamnt~1,family=binomial,data=tr)
summary(fitLog)
library(lmtest)
lrtest(fitLog,fitNull)




detach(tr)
