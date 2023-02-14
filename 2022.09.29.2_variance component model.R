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
  m2.est<-numeric(0)
  m2.wald<- numeric(0)
  m2.ll<- numeric(0)
  m2.LRT<- numeric(0)
  m2.std<- numeric(0)
  m2.conv.fail<-numeric(0)
  m2.aic <-numeric(0)
  m2.bic <-numeric(0)
  
  ############################################
  ####      for loop(2): replication    ######
  ############################################
  for (n in 1:nrep) { #n: replications
    
    dat<- read.table(file=paste("dv_cond",cond,"_",n,"rep.txt",sep=""), header=T)
    
    ############################## Beginning of LMM
    #random intercept only model
    m2     =lmer(y~1+x1+x2+x3+(1+x1+x2+x3||j)+(1|i), data=dat, REML=F) 
    m2.null=lmer(y~1+x1+x2   +(1+x1+x2+x3||j)+(1|i), data=dat, REML=F)# random slope
    
    ################################################
    ### several things to save
    
    # fixed effect
    B3est <-summary(m2)$coefficients[c(4)]
    m2.est<- rbind(m2.est,B3est)
    
    #standard error (new)
    std.err<-summary(m2)$coefficients[c(8)]
    m2.std<- rbind(m2.std,std.err)
    
    # wald
    B3wald <- summary(m2)$coefficients[c(12)]
    m2.wald<- rbind(m2.wald, B3wald)
    
    # Log-likelihood
    llm2<-summary(m2)$logLik
    m2.ll <-rbind(m2.ll,llm2)
    
    # LRT
    lrt2<- anova(m2,m2.null)[2,6]   #(null model, proposed model )*-2
    m2.LRT<-rbind(m2.LRT, lrt2)
    
    #convergence successed?
    conv2<- ifelse(!is.null(m2@optinfo$conv$lme4$messages)&&
                     grepl('failed to converge', m2@optinfo$conv$lme4$messages),1,0)
    m2.conv.fail<-rbind(m2.conv.fail,conv2)
    
    # information criteria
    aic2 <- summary(m2)$AIC[c(1)]
    m2.aic<- rbind(m2.aic, aic2)
    bic2 <- summary(m2)$AIC[c(2)]
    m2.bic<- rbind(m2.bic, bic2)
    
    
    ################################################
    ### several things to save
    ################################################
    cat("\n",cond,'cond, ',n,'rep')
    
    #make list
    outFromM2<-data.frame(m2.est, m2.std, m2.wald, m2.ll, m2.LRT, m2.conv.fail, m2.aic, m2.bic)
    
    M2<-data.frame(outFromM2)
    M2$m2wald.Rate<-ifelse(abs(M2$m2.wald)>1.96,1,0)#자유도 1 t 기준을 넘기는가.
    M2$m2lrt.Rate<-ifelse(M2$m2.LRT>3.84,1,0)  #자유도 1 카이제곱 기준을 넘기는가.
    
    ###############################
  } # loop 2, replication   #####
  ###############################

    # The thing to save for random intercept model
    write.table(M2, file=paste("M2out_cond",cond,"_",nrep,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)

  #################################
} #loop 3, condition    ########
###############################

