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
  
  ###### spaces to save
  m1.est<-numeric(0)
  m1.wald<- numeric(0)
  m1.ll<- numeric(0)
  m1.LRT<- numeric(0)
  m1.std<- numeric(0)
  m1.conv.fail<-numeric(0)
  m1.aic <-numeric(0)
  m1.bic <-numeric(0)

  ############################################
  ####      for loop(2): replication    ######
  ############################################
    for (n in 1:nrep) { #n: replications
    
    dat<- read.table(file=paste("dv_cond",cond,"_",n,"rep.txt",sep=""), header=T)
    
    ############################## Beginning of LMM
    #random intercept only model
    m1     =lmer(y~1+x1+x2+x3+(1|j)+(1|i), data=dat, REML=F) 
    m1.null=lmer(y~1+x1+x2   +(1|j)+(1|i), data=dat, REML=F)# random intercept-only
    
    ################################################
    ### several things to save
    
    # fixed effect
    B3est <-summary(m1)$coefficients[c(4)]
    m1.est<- rbind(m1.est,B3est)
    
    #standard error (new)
    std.err<-summary(m1)$coefficients[c(8)]
    m1.std<-rbind(m1.std,std.err)
    
    # wald
    B3wald <- summary(m1)$coefficients[c(12)]
    m1.wald<- rbind(m1.wald, B3wald)
    
    # Log-likelihood
    llM1<-summary(m1)$logLik
    m1.ll <-rbind(m1.ll,llM1)
    
    # LRT
    lrt1<- anova(m1,m1.null)[2,6]   #(null model, proposed model )*-2
    m1.LRT<-rbind(m1.LRT, lrt1)
    
    #convergence successed?
    conv1<- ifelse(!is.null(m1@optinfo$conv$lme4$messages)&&
                     grepl('failed to converge', m1@optinfo$conv$lme4$messages),1,0)
    m1.conv.fail<-rbind(m1.conv.fail,conv1)
    
    # information criteria
    aic1<- summary(m1)$AIC[c(1)]
    m1.aic <-rbind(m1.aic, aic1)
    bic1<- summary(m1)$AIC[c(2)]
    m1.bic <-rbind(m1.bic, bic1)
    

    ################################################
    ### several things to save
    ################################################
    cat("\n",cond,'cond, ',n,'rep')
    
    #make list
    
    outFromM1<-data.frame(m1.est, m1.std, m1.wald, m1.ll, m1.LRT, m1.conv.fail, m1.aic, m1.bic)
    
    M1<-data.frame(outFromM1)
    M1$M1wald.Rate<-ifelse(abs(M1$m1.wald)>1.96,1,0)#자유도 1 t 기준을 넘기는가.
    M1$M1lrt.Rate<-ifelse(M1$m1.LRT>3.84,1,0)  #자유도 1 카이제곱 기준을 넘기는가.
    
    ###############################
  } # loop 2, replication   #####
  ###############################
  
  # The thing to save for random intercept model
  write.table(M1, file=paste("M1out_cond",cond,"_",nrep,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)
  
  #################################
} #loop 3, condition    ########
###############################

