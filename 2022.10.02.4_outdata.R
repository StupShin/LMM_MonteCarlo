#### Set directories
#path<- ("c:/users/wooyeol/dropbox/workplace/research/LMM-SIM/GLMM/20220103/")
path<- ('/Users/shin/Desktop/R_LMM')
#path<- ("R:/LMM")
#path<- ("C:/LMM")
#path<- ("D:/LMM")


## packages
library(MASS)
library(lme4)
library(dplyr)
library(readxl)

##### Set paths  
#1) Desktop
path<-(path)

### nrep
nrep=1000

### make place to append
dataPR<- numeric(0)
dataM1<- numeric(0)
dataM2<- numeric(0)
dataM3<- numeric(0)

pick.aic.m1<-numeric(0)
pick.aic.m2<-numeric(0)
pick.aic.m3<-numeric(0)

pick.bic.m1<-numeric(0)
pick.bic.m2<-numeric(0)
pick.bic.m3<-numeric(0)

lrt.forward.1<-numeric(0)
lrt.forward.2<-numeric(0)
lrt.forward.3<-numeric(0)


lrt.m1m3.n.1<-numeric(0)
lrt.m1m3.n.2<-numeric(0)
lrt.m1m3.n.3<-numeric(0)

lrt.m1m3.m.1<-numeric(0)
lrt.m1m3.m.2<-numeric(0)
lrt.m1m3.m.3<-numeric(0)

aic.wald.mc<-numeric(0)
bic.wald.mc<-numeric(0)

deviance.m1m2.95<-numeric(0)
deviance.m2m3.95<-numeric(0)
deviance.m1m3.95<-numeric(0)


forward.wald.mc<-numeric(0)
m1m3.wald.mc<-numeric(0)


### get info
setwd(path)
condinfo<- read_excel(path = 'condition_info.xlsx')


########################################
##### for loop(3): condition ###########
########################################
for (cond in 1:54){
  
  ###### go to dir to work
  setwd(path)
  setwd(paste0("cond",cond))
  
  #load data
#  datPR<- read.table(file=paste("ParameterRecovery_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datM1<- read.table(file=paste("M1out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datM2<- read.table(file=paste("M2out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datM3<- read.table(file=paste("M3out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datMC<- read.table(file=paste("Comparison_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  
  
  ### summary
  # parameter recovery
#  dataPR<- rbind(dataPR,datPR)
  
  # random intercept model
  datM1.mean <- apply(datM1[,c(1:10)],2,mean)
  dataM1<- rbind(dataM1,datM1.mean)
  
  # variance component model
  datM2.mean <- apply(datM2[,c(1:10)],2,mean)
  dataM2<- rbind(dataM2,datM2.mean)
  
  # unstructured model
  datM3.mean <- apply(datM3[,c(1:10)],2,mean)
  dataM3<- rbind(dataM3,datM3.mean)
  
  
  # pick, aic
  pick.aic.1<- nrow(filter(.data = datMC, datMC$aic=="m1.aic"))
  pick.aic.m1<-rbind(pick.aic.m1,pick.aic.1)
  pick.aic.2<- nrow(filter(.data = datMC, datMC$aic=="m2.aic"))
  pick.aic.m2<-rbind(pick.aic.m2,pick.aic.2)
  pick.aic.3<- nrow(filter(.data = datMC, datMC$aic=="m3.aic"))
  pick.aic.m3<-rbind(pick.aic.m3,pick.aic.3)
  
  # pick, bic
  pick.bic.1<- nrow(filter(.data = datMC, datMC$bic=="m1.bic"))
  pick.bic.m1<-rbind(pick.bic.m1,pick.bic.1)
  pick.bic.2<- nrow(filter(.data = datMC, datMC$bic=="m2.bic"))
  pick.bic.m2<-rbind(pick.bic.m2,pick.bic.2)
  pick.bic.3<- nrow(filter(.data = datMC, datMC$bic=="m3.bic"))
  pick.bic.m3<-rbind(pick.bic.m3,pick.bic.3)
  
  # deviance
  devi.m1m2<- quantile(datMC$deviance.m1m2,.95)
  deviance.m1m2.95<-rbind(deviance.m1m2.95,devi.m1m2)
  devi.m2m3<- quantile(datMC$deviance.m2m3,.95)
  deviance.m2m3.95<-rbind(deviance.m2m3.95,devi.m2m3)
  devi.m1m3<- quantile(datMC$deviance.m1m3,.95)
  deviance.m1m3.95<-rbind(deviance.m1m3.95,devi.m1m3)

  # pick, forward selection
  pick.lrt.forward.1<- nrow(filter(.data = datMC, datMC$which.forward=="m1"))
  lrt.forward.1 <-rbind(lrt.forward.1,pick.lrt.forward.1)
  pick.lrt.forward.2<- nrow(filter(.data = datMC, datMC$which.forward=="m2"))
  lrt.forward.2 <-rbind(lrt.forward.2,pick.lrt.forward.2)
  pick.lrt.forward.3<- nrow(filter(.data = datMC, datMC$which.forward=="m3"))
  lrt.forward.3 <-rbind(lrt.forward.3,pick.lrt.forward.3)
  
  # pick, intercept only model and unstructured model
  pick.lrt.m1m3.n.1<- nrow(filter(.data = datMC, datMC$which.m1m3=="m1"))
  lrt.m1m3.n.1 <-rbind(lrt.m1m3.n.1,pick.lrt.m1m3.n.1)
  pick.lrt.m1m3.n.2<- nrow(filter(.data = datMC, datMC$which.m1m3=="m2"))
  lrt.m1m3.n.2 <-rbind(lrt.m1m3.n.2,pick.lrt.m1m3.n.2)
  pick.lrt.m1m3.n.3<- nrow(filter(.data = datMC, datMC$which.m1m3=="m3"))
  lrt.m1m3.n.3 <-rbind(lrt.m1m3.n.3,pick.lrt.m1m3.n.3)
  
  aic.wald.rate<-mean(datMC$aic.wald)
  aic.wald.mc <-rbind(aic.wald.mc,aic.wald.rate)
  bic.wald.rate<-mean(datMC$bic.wald)
  bic.wald.mc <-rbind(bic.wald.mc,bic.wald.rate)
  forward.wald.rate<-mean(datMC$forward.wald)
  forward.wald.mc<-rbind(forward.wald.mc,forward.wald.rate)
  m1m3.wald.rate<-mean(datMC$m1m3.wald)
  m1m3.wald.mc<-rbind(m1m3.wald.mc,m1m3.wald.rate)
  
  
  # model comparison

  outdata1<-cbind(dataPR,dataM1,dataM2,dataM3)
  outdata2<-cbind(pick.aic.m1, pick.aic.m2, pick.aic.m3,
                 pick.bic.m1, pick.bic.m2, pick.bic.m3,
                 deviance.m1m2.95, deviance.m2m3.95, deviance.m1m3.95,
                 lrt.forward.1,lrt.forward.2,lrt.forward.3,
                 lrt.m1m3.n.1,lrt.m1m3.n.2,lrt.m1m3.n.3,
                 aic.wald.mc,bic.wald.mc, forward.wald.mc,m1m3.wald.mc
                 )

  #################################
} #loop 3, condition    ########
###############################



colnames(outdata2)<-c('pick.aic.m1', 'pick.aic.m2', 'pick.aic.m3',
                'pick.bic.m1', 'pick.bic.m2', 'pick.bic.m3',
                '95%.deviance.m1m2', '95%.deviance.m2m3', '95%.deviance.m1m3',
                'lrt.forward.1','lrt.forward.2','lrt.forward.3',
                'lrt.m1m3.n.1','lrt.m1m3.n.2','lrt.m1m3.n.3',
                'aic.wald.mc','bic.wald.mc', 'forward.wald.mc','m1m3.wald.mc')

outdata<-data.frame(cbind(outdata1,outdata2))

# The thing to save
setwd(path)
condinfo<-condinfo[c(1:cond),]
outdata<- cbind(condinfo,outdata)
writexl::write_xlsx(outdata, path = 'outdata.xlsx',col_names=TRUE)