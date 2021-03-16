source("M:/Environmental Data Summary/HelperCode/prioritize_on.R")

Ntest=100
TEST=data.frame(ON=rnorm(Ntest),OFF1=rnorm(Ntest),OFF2=rnorm(Ntest),OFF3=rnorm(Ntest))
select=sample(1:Ntest,10)
colors=rep("black",Ntest)
colors[select]="red"
pairs(TEST,col=colors,pch=16)

df=TEST
on="ON"
off=c("OFF1","OFF2","OFF3")
seed=1



Ntest=1000
TEST=data.frame(OFF1=rnorm(Ntest),OFF2=rnorm(Ntest),OFF3=rnorm(Ntest),ON=rnorm(Ntest))
si=prioritize_on(df=TEST,on = "ON",off=c("OFF1","OFF2","OFF3"), seed=1,DEBUG = T,Nselect = 30,Ntries = 10)
colors=rep("black",Ntest)
colors[si]="red"
cexs=rep(.5,Ntest)
cexs[si]=2
pairs(TEST,col=colors,pch=16,cex=cexs)
