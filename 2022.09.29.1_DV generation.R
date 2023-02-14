#### Set directories
#path<- ("c:/users/wooyeol/dropbox/workplace/research/LMM-SIM/GLMM/20220125")
path<- ('/Users/shin/Desktop/R_LMM')
#path<- ("R:/LMM")
#path<- ("C:/LMM")
#path<-("D:/LMM")

### set.seed
set.seed(7654321)

## packages
library(MASS)
library(lme4)


##### Set paths  
#1) Desktop
path<-(path)

### nrep
nrep=1000

## 2022-09-20: Two experimental condition with two levels 
### Varying conditions
# tau^2 0.3^2, 0.5^2, 0.7^2
# cov matrix.  int_only, var_comp, Unstr




### Fixed conditions
# number of persons: 30
# number of items per level: 10
# beta_0 beta_1 _beta_2 = 0;
# beta_3 = 0.5


# person random effect: 
# tau0(unit=variance): 0.7^2
# corr(tau): 0.7


# item random effect(sd): 
# b0=b1=b2=0
# omega 0.7 
# sigma(=sd(e)): 1

##  Fixed conditions
beta0=0;beta1=0;beta2=0;

##  Info about cond (fixed effects)
nperson.v=c(rep(30,18),rep(60,18),rep(120,18))
nitem.v  =c(rep(10,54))
beta.v   =rep(c(rep(0,9),rep(0.2,9)),3)

##  Info about cond (random effects)
tau.v    =rep(c('Int.only','VarComp','UNstr'),18)
omega.v  =c(rep(c(0.49),54))

##  Residual of model
sigma=1

##  total number of dataset
nDV<- nperson.v*nitem.v*4

##  Person random effects, was.
tau0.v=rep(c(rep(0.3^2,3), rep(0.5^2,3), rep(0.7^2,3)),6)



#tau.int= matrix(c(tau0,0,0,0), 
#                nrow=2,ncol=2)
#tau.slp= matrix(c(tau0,tau0*(0.7),tau0*0.7,tau0),  
#                nrow=2,ncol=2)

##  Person random effects, now.
#'UNstr','ComSymm','VarComp','IDen'




########################################
##### for loop(3): condition ###########
########################################
for (cond in 1:54){
  
  nperson=nperson.v[cond]
  nitem=nitem.v[cond]
  beta3= beta.v[cond]
  tau0<-tau0.v[cond]
  
  Int.only= matrix(c(tau0*1, tau0*0, tau0*0, tau0*0,
                     tau0*0, tau0*0, tau0*0, tau0*0,
                     tau0*0, tau0*0, tau0*0, tau0*0,
                     tau0*0, tau0*0, tau0*0, tau0*0),  
                   nrow=4,ncol=4)
  
  VarComp = matrix(c(tau0,   tau0*0,    tau0*0,     tau0*0,
                     tau0*0, tau0*(0.8),tau0*0,     tau0*0,
                     tau0*0, tau0*0,    tau0*(0.64),tau0*0,
                     tau0*0, tau0*0,    tau0*0,     tau0*(0.512)),  
                   nrow=4,ncol=4)
  
  UNstr = matrix(c(tau0,       tau0*(0.3), tau0*0.7,   tau0*(-0.3),
                   tau0*0.3,   tau0,       tau0*0.7,   tau0*(-0.3),
                   tau0*0.7,   tau0*0.7,   tau0,       tau0*(-0.7),
                   tau0*(-0.3),tau0*(-0.3),tau0*(-0.7),tau0),  
                 nrow=4,ncol=4)
  
  
  
  tau=tau.v[cond]
  omega = omega.v[cond]


  ############################### 
  ### indicators
  x0= c(rep(1,nperson*nitem*4)) ## intercept
  x1= c(rep(0,nperson*nitem),rep(1,nperson*nitem),rep(0,nperson*nitem),rep(1,nperson*nitem))
  x2= c(rep(0,nperson*nitem),rep(0,nperson*nitem),rep(1,nperson*nitem),rep(1,nperson*nitem))
  x3= x1*x2  #b3와 짝꿍.각 수준의 상호작용  
  lv=factor(c(rep(0,nperson*nitem),rep(1,nperson*nitem),rep(2,nperson*nitem),rep(3,nperson*nitem)))
  
  j=factor(rep(rep(c(1:nperson),each=nitem),4)) 
  i=factor(rep(rep(c(1:nitem),nperson),4)) 

  
  ############################################
  ####      for loop(2): replication    ######
  ############################################
  
  for (n in 1:nrep) { #n: replications
    ## generate random effects
    #person random
    z.s= mvrnorm(nperson, mu=c(0,0,0,0), Sigma=get(tau))
    
    s0= rep(rep(z.s[,1],each=nitem),4)*x0
    s1= rep(rep(z.s[,2],each=nitem),4)*x1
    s2= rep(rep(z.s[,3],each=nitem),4)*x2
    s3= rep(rep(z.s[,4],each=nitem),4)*x3
    
    #item random
    ### New: item random effect as vector
    w1=rnorm(nitem,mean=0, sd=omega)
    w= rep(rep(w1,nperson),4) 
    
    #residual of model
    e=rnorm(nperson*nitem*4, mean=0, sd=sigma)   

    
    #dependent variable
    y= (beta0*x0+ beta1*x1+ beta2*x2+ beta3*x3)+ 
      s0+s1+s2+s3+ w+e
    
    #data will be used
    dat<- data.frame(nperson,nitem,beta3,
                     omega, j, i,
                     x0,x1,x2,x3,lv, 
                     s0,s1,s2,s3,w,e,y) 
    
    ############################## END of Data generation  
    ################################################
    ### thing to save
    
    #make directory
    setwd(path)
    dir.create(paste0("cond",cond)) ## 새로운 폴더 생성 
    setwd(paste0("cond",cond)) ## 새로운 경로로 setting 
    cat("\n",cond,'cond, ',n,'rep')
    
    write.table(dat, file=paste("dv_cond",cond,"_",n,"rep.txt",sep=""),sep="\t",row.names = F,col.names = T)
    
    ###############################
  } # loop 2, replication   #####
  ###############################
  #################################
} #loop 3, condition    ########
###############################

setwd(path)
condnum<-c(1:54)
condarray<- data.frame(condnum, nperson.v,nitem.v,beta.v,tau0.v,tau.v,omega.v,nDV)
writexl::write_xlsx(condarray,path = 'condition_info.xlsx')
