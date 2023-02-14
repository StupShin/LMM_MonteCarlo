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
nrep=1000

########################################
##### for loop(3): condition ###########
########################################
for (cond in 1:54){
  
  ###### go to dir to work
  setwd(path)
  setwd(paste0("cond",cond))
  
    #load data
    datM1<- read.table(file=paste("M1out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T) #int only
    datM2<- read.table(file=paste("M2out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T) #var comp
    datM3<- read.table(file=paste("M3out_cond",cond,"_",nrep,"rep.txt",sep=""), header=T) #unstr
    
    #get log likelihood from two models
    m1.ll<- datM1$m1.ll
    m2.ll<- datM2$m2.ll
    m3.ll<- datM3$m3.ll
    
    #aic
    m1.aic<- datM1$AIC
    m2.aic<- datM2$AIC
    m3.aic<- datM3$AIC
    aic<-ifelse(m1.aic < m3.aic,ifelse(m1.aic < m2.aic, 'm1.aic','m2.aic'), ifelse(m3.aic<m2.aic,'m3.aic','m2.aic'))
    aic.wald<- ifelse(aic=='m1.aic',datM1$M1wald.Rate,
                      ifelse(aic=='m2.aic', datM2$m2wald.Rate, datM3$m3wald.Rate))
    
    #bic
    m1.bic<- datM1$BIC
    m2.bic<- datM2$BIC
    m3.bic<- datM3$BIC
    bic<-ifelse(m1.bic < m3.bic,ifelse(m1.bic < m2.bic, 'm1.bic','m2.bic'), ifelse(m3.bic<m2.bic,'m3.bic','m2.bic'))
    bic.wald<- ifelse(bic=='m1.bic',datM1$M1wald.Rate,
                      ifelse(bic=='m2.bic', datM2$m2wald.Rate, datM3$m3wald.Rate))

    #deviance
    ll.diff.21<-m1.ll-m2.ll
    deviance.m1m2<-(-2)*ll.diff.21

    ll.diff.32<-m2.ll-m3.ll
    deviance.m2m3<-(-2)*ll.diff.32
    
    ll.diff.31<-m1.ll-m3.ll
    deviance.m1m3<-(-2)*ll.diff.31
    
    #pick, m1 vs. m2
    pick12 <-ifelse(deviance.m1m2 > qchisq(0.95,3),1,0)
    which.m1m2 <-ifelse(pick12==1,'m2','m1')
    
    #pick, m2 vs. m3
    pick23 <-ifelse(deviance.m2m3 > qchisq(0.95,6),1,0)
    which.m2m3 <-ifelse(pick23==1,'m3','m2')
    
    which.forward<-ifelse(which.m1m2=='m1','m1',
                          ifelse(which.m2m3=='m2','m2','m3'))
    forward.wald<-ifelse(which.forward=='m1',datM1$M1wald.Rate,
                         ifelse(which.forward=='m2',datM2$m2wald.Rate, datM3$m3wald.Rate))
    
    #pick, m1 vs. m3, normal
    pick13.n <-ifelse(deviance.m1m3 > qchisq(0.95,9),1,0)
    which.m1m3 <-ifelse(pick13.n==1,'m3','m1')
    m1m3.wald<- ifelse(which.m1m3=='m1',datM1$M1wald.Rate, datM3$m3wald.Rate)
    
    #pick, m2 vs. m3, mixture
#    pick13.m <-ifelse(ll.devi31 > (qchisq(0.95,6)+qchisq(0.95,9))/2,1,0)
#    which.m1m3.mixture <-ifelse(pick13.m==1,'m3','m1')
    
    cat("\n",cond)
    
    #make list
    comp<-data.frame(aic, aic.wald, bic, bic.wald, which.forward, forward.wald, which.m1m3, m1m3.wald, deviance.m1m2, deviance.m2m3, deviance.m1m3)


    # The thing to save
    write.table(comp, file=paste("Comparison_cond",cond,"_",nrep,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)
    
  #################################
} #loop 3, condition    ########
###############################

