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

##### Set paths  
#1) Desktop
path<-(path)

### nrep
nrep=50

### make space to save
m1.bias<-numeric(0)
m2.bias<-numeric(0)
m1.rmse<-numeric(0)
m2.rmse<-numeric(0)
m1.std.data<-numeric(0)
m2.std.data<-numeric(0)

### True beta 3
beta.v = c(rep(0,15),rep(0.5,15))

########################################
##### for loop(3): condition ###########
########################################
for (cond in 1:30){
  
  ###### go to dir to work
  setwd(path)
  setwd(paste0("cond",cond))
  
  #load data
  datM1<- read.table(file=paste("M1out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datM2<- read.table(file=paste("M2out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  datM3<- read.table(file=paste("M3out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T)
  
  #true beta3 per condition
  beta3= beta.v[cond]
  
  #get bias
  m1.bias<-mean(datM1$m1.est)-beta3
  m2.bias<-mean(datM2$m2.est)-beta3
  m3.bias<-mean(datM3$m3.est)-beta3
  
  #get standard deviation
  m1.std.data<-sd(datM1$m1.est-beta3)
  m2.std.data<-sd(datM2$m2.est-beta3)
  m3.std.data<-sd(datM3$m3.est-beta3)
  
  #standard error, std error from model.
  m1.std.model<-mean(datM1$m1.std)
  m2.std.model<-mean(datM2$m2.std)
  m3.std.model<-mean(datM3$m3.std)

  # bias. from standard deviation from data and standard error from model
  m1.bias.std<- m1.std.data-m1.std.model
  m2.bias.std<- m2.std.data-m2.std.model
  m3.bias.std<- m3.std.data-m3.std.model
  
  #confidence interval
  lower1 <- datM1$m1.est-(1.96*datM1$m1.std)
  upper1 <- datM1$m1.est+(1.96*datM1$m1.std)
  confi.m1 <-ifelse(lower1<beta3,ifelse(upper1>beta3,1,0),0)
  confi.m1 <-mean(confi.m1)
  
  lower2 <- datM2$m2.est-(1.96*datM2$m2.std)
  upper2 <- datM2$m2.est+(1.96*datM2$m2.std)
  confi.m2 <-ifelse(lower2<beta3,ifelse(upper2>beta3,1,0),0)
  confi.m2 <-mean(confi.m2)
  
  lower3 <- datM3$m3.est-(1.96*datM3$m3.std)
  upper3 <- datM3$m3.est+(1.96*datM3$m3.std)
  confi.m3 <-ifelse(lower3<beta3,ifelse(upper3>beta3,1,0),0)
  confi.m3 <-mean(confi.m3)
  
  
  cat("\n",cond)
  
  #make list
  p.recov<-data.frame(m1.bias, m2.bias, m3.bias, 
                      m1.std.data, m2.std.data,m3.std.data,
                      m1.std.model,m2.std.model,m3.std.model,
                      m1.bias.std,m2.bias.std,m3.bias.std,
                      confi.m1,confi.m2,confi.m3)
                      
  # The thing to save
  write.table(p.recov, file=paste("ParameterRecovery_cond",cond,"_",nrep,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)
  
  #################################
} #loop 3, condition    ########
###############################

