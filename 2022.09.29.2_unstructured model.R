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
  m3.est<-numeric(0)
  m3.wald<- numeric(0)
  m3.ll<- numeric(0)
  m3.LRT<- numeric(0)
  m3.std<- numeric(0)
  m3.conv.fail<-numeric(0)
  m3.aic<- numeric(0)
  m3.bic<- numeric(0)

  ############################################
  ####      for loop(2): replication    ######
  ############################################
  for (n in 1:nrep) { #n: replications
    
    dat<- read.table(file=paste("dv_cond",cond,"_",n,"rep.txt",sep=""), header=T)
    
    ############################## Beginning of LMM
    #random intercept only model
    m3     =lmer(y~1+x1+x2+x3+(1+x1+x2+x3|j)+(1|i), data=dat, REML=F) 
    m3.null=lmer(y~1+x1+x2   +(1+x1+x2+x3|j)+(1|i), data=dat, REML=F)# random slope
    
    ################################################
    ### several things to save
    
    # fixed effect
    B3est <-summary(m3)$coefficients[c(4)]
    m3.est<- rbind(m3.est,B3est)
    
    #standard error (new)
    std.err<-summary(m3)$coefficients[c(8)]
    m3.std<- rbind(m3.std,std.err)
    
    # wald
    B3wald <- summary(m3)$coefficients[c(12)]
    m3.wald<- rbind(m3.wald, B3wald)
    
    # Log-likelihood
    llm3<-summary(m3)$logLik
    m3.ll <-rbind(m3.ll,llm3)
    
    # LRT
    lrt3<- anova(m3,m3.null)[2,6]   #(null model, proposed model )*-2
    m3.LRT<-rbind(m3.LRT, lrt3)
    
    #convergence successed?
    conv3<- ifelse(!is.null(m3@optinfo$conv$lme4$messages)&&
                     grepl('failed to converge', m3@optinfo$conv$lme4$messages),1,0)
    m3.conv.fail<-rbind(m3.conv.fail,conv3)
    
    # information criteria
    aic3 <- summary(m3)$AIC[c(1)]
    m3.aic<- rbind(m3.aic, aic3)
    bic3 <- summary(m3)$AIC[c(2)]
    m3.bic<- rbind(m3.bic, bic3)
    

    ################################################
    ### several things to save
    ################################################
    cat("\n",cond,'cond, ',n,'rep')
    
    #make list
    outFromM3<-data.frame(m3.est, m3.std, m3.wald, m3.ll, m3.LRT, m3.conv.fail, m3.aic, m3.bic)
    
    M3<-data.frame(outFromM3)
    M3$m3wald.Rate<-ifelse(abs(M3$m3.wald)>1.96,1,0)#자유도 1 t 기준을 넘기는가.
    M3$m3lrt.Rate<-ifelse(M3$m3.LRT>3.84,1,0)  #자유도 1 카이제곱 기준을 넘기는가.
    
    ###############################
  } # loop 2, replication   #####
  ###############################
  
  # The thing to save for random intercept model
  write.table(M3, file=paste("M3out_cond",cond,"_",nrep,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)
  
  #################################
} #loop 3, condition    ########
###############################

