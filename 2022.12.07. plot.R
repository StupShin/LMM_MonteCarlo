setwd('R:/')

pacman::p_load(ggplot2,tidyr,dplyr,ggbreak)

convergence_dat<-readxl::read_xlsx('2022.12.22.table.xlsx',sheet=1)
selection_dat<-readxl::read_xlsx('2022.12.22.table.xlsx',sheet=2)
percentile95_dat<-readxl::read_xlsx('2022.12.22.table.xlsx',sheet=3)
detection_dat<-readxl::read_xlsx('2022.12.22.table.xlsx',sheet=4)


#####

#true model selection
#####
#all
selection_all<- gather(selection_dat,key='method',value='N_selection',c('lrt.m1','lrt.m2','lrt.m3','aic.m1','aic.m2','aic.m3','bic.m1','bic.m2','bic.m3'))
selection_all$model<- rep(c(rep('무선절편모형',54),rep('분산성분모형',54),rep('비구조모형',54)),3)
selection_all$strategy<- c(rep('LRT',162),rep('AIC',162),rep('BIC',162))

#selected, when var =0.09
selection_0.09<- selection_all%>% filter(tau==0.09)
ggplot(selection_0.09, aes(x=factor(participant), y=N_selection, fill=strategy)) +
  geom_bar(stat='identity', position='dodge')+
  facet_grid(factor(model, levels = c('무선절편모형','분산성분모형','비구조모형')) 
             ~ 공분산구조)+
  labs(x='참가자 수', y='각 모형을 선택한 비율', fill='모형선택전략')+
  scale_fill_discrete(labels=c('AIC','BIC','우도비검정'))
  

#selected var comp
selection_0.25<- selection_all%>% filter(tau==0.25)
ggplot(selection_0.25, aes(x=factor(participant), y=N_selection, fill=strategy)) +
  geom_bar(stat='identity', position='dodge')+
  facet_grid(factor(model, levels = c('무선절편모형','분산성분모형','비구조모형')) 
             ~ 공분산구조)+
  labs(x='참가자 수', y='각 모형을 선택한 비율', fill='모형선택전략')+
  scale_fill_discrete(labels=c('AIC','BIC','우도비검정'))


#selected unstr
selection_0.49<- selection_all%>% filter(tau!=0.09) %>% filter(tau!=0.25)
ggplot(selection_0.49, aes(x=factor(participant), y=N_selection, fill=strategy)) +
  geom_bar(stat='identity', position='dodge')+
  facet_grid(factor(model, levels = c('무선절편모형','분산성분모형','비구조모형')) 
             ~ 공분산구조)+
  labs(x='참가자 수', y='각 모형을 선택한 비율', fill='모형선택전략')+
  scale_fill_discrete(labels=c('AIC','BIC','우도비검정'))


#####

#95 percentile
percentile95_dat<-percentile95_dat[c(1:18),]
percentile_all<- gather(percentile95_dat,key='research_model',value='percentile95_D',c('vs.VarComp','vs.Unstr'))
percentile_all$perc95<- c(rep(7.814,18),rep(16.918,18))

ggplot(percentile_all, aes(x=factor(participant), y=percentile95_D, fill=factor(tau)))+
  geom_bar(stat='identity',position='dodge')+
  facet_grid(.~factor(research_model, levels=c('vs.VarComp','vs.Unstr'),labels=c('분산성분 모형과 비교 (기각값: 7.814)','비구조 모형과 비교 (기각값: 16.918)')))+
  labs(x='참가자 수', y='무선절편 모형과 비교했을 때 산출된 D 값의 95분위수')+
  geom_hline(aes(yintercept=perc95),colour='red',size=1)+
  scale_fill_discrete(name="참가자 무선효과의\n분산",labels=c('0.09','0.25','0.49'))


#####
#detection rate
detection_dat<-detection_dat[,c(1:4,7:10)]
detection_dat<- gather(detection_dat,key='strategy',value='detection_rate',c('maximal','AIC','BIC','forward.lrt'))
#type 1 error
detection_type1<-detection_dat %>% filter(beta3==0)
ggplot(detection_type1, aes(x=factor(strategy),y=detection_rate))+
  geom_point(aes(colour=factor(tau)))+
  geom_line(aes(group = factor(tau),colour=factor(tau)))+
  facet_grid(factor(cov_matrix, levels = c('무선절편','분산성분','비구조'))
             ~factor(participant, levels = c('30','60','120'), labels=c('참가자수: 30명','참가자수: 60명','참가자수: 120명')))+
  scale_color_discrete(name=c('참가자 무선효과의\n분산'),labels=c('0.09','0.25','0.49'))+
  scale_x_discrete(labels=c('AIC','BIC','우도비검정','최대모형'))+
  labs(x='모형 선택 전략', y='1종 오류 비율')+
  geom_hline(yintercept=0.05,colour='red')


#statistical power
detection_power<-detection_dat %>% filter(beta3==0.2)
ggplot(detection_power, aes(x=factor(strategy),y=detection_rate))+
  geom_point(aes(colour=factor(tau)))+
  geom_line(aes(group = factor(tau),colour=factor(tau)))+
  facet_grid(factor(cov_matrix, levels = c('무선절편','분산성분','비구조'))
             ~factor(participant, levels = c('30','60','120'), labels=c('참가자수: 30명','참가자수: 60명','참가자수: 120명')))+
  scale_color_discrete(name=c('참가자 무선효과의\n분산'),labels=c('0.09','0.25','0.49'))+
  scale_x_discrete(labels=c('AIC','BIC','우도비검정','최대모형'))+
  labs(x='모형 선택 전략', y='통계적 검정력')+
  geom_hline(yintercept=0.8,colour='blue')


