#SPS dataset analysis
sps=read.csv(here::here('data','raw data','multispecies','SPS_2022 Biological Viability Assessment.csv'),na.strings = c('-99','-99.00'))
sps2=read.csv(here::here('data','raw data','multispecies','SPS Abundance - Population data and references for the Salmon Population Summary (SPS) Database Data.csv'),na.strings = c('-99','-99.00'))

#sps_sub=subset(sps,Effective.Catch>0)
sps_sub=subset(sps,SPECIES!='Steelhead')
sps_sub=subset(sps_sub,SPECIES!='steelhead')
sps_sub=subset(sps_sub,CATCH>=0)
sps_sub=subset(sps_sub,POPULATION_NAME!='Mid-Hood Canal Chinook') #drop this stock, as some a number of age proportions are off (e.g. negative proportions)
sps_sub=subset(sps_sub,POPULATION_NAME!='Skykomish Chinook (Snohomish)') #drop this stock, not enough data


unique(paste(sps_sub$POPULATION_NAME,sps_sub$ESU,sep='_'))

bt_list=list()
for(i in 1:length(unique(sps_sub$POPULATION_NAME))){
  s=subset(sps_sub,POPULATION_NAME==levels(factor(sps_sub$POPULATION_NAME))[i])
  s=s[order(s$YEAR),]
  s$RUN_SIZE=s$NUMBER_OF_SPAWNERS*s$FRACWILD+s$CATCH
  s=s[is.na(s$FRACWILD)==F,]
  
  if(nrow(s)==0){next}
  
  bt_temp=data.frame(stock=unique(s$POPULATION_NAME),broodyear=s$YEAR,run=round(s$RUN_SIZE),spawners=round(s$NUMBER_OF_SPAWNERS),recruits=NA,r1=NA,r2=NA,r3=NA,r4=NA,r5=NA,r6=NA,r7=NA,p.wild=s$FRACWILD)
  for(t in 1:nrow(s)){
  
  if(is.na(match(s$YEAR[t]-1,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-1,bt_temp$broodyear),6]=round(s$RUN_SIZE[t]*s$AGE_1_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-2,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-2,bt_temp$broodyear),7]=round(s$RUN_SIZE[t]*s$AGE_2_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-3,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-3,bt_temp$broodyear),8]=round(s$RUN_SIZE[t]*s$AGE_3_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-4,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-4,bt_temp$broodyear),9]=round(s$RUN_SIZE[t]*s$AGE_4_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-5,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-5,bt_temp$broodyear),10]=round(s$RUN_SIZE[t]*s$AGE_5_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-6,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-6,bt_temp$broodyear),11]=round(s$RUN_SIZE[t]*s$AGE_6_RETURNS[t])
  }
  if(is.na(match(s$YEAR[t]-7,bt_temp$broodyear))==F){
    bt_temp[match(s$YEAR[t]-7,bt_temp$broodyear),12]=round(s$RUN_SIZE[t]*s$AGE_7_RETURNS[t])
  }
  
  }
  if(all(is.na(bt_temp[,11])==T)&all(is.na(bt_temp[,12])==T)){
    bt_temp$recruits=apply(bt_temp[,6:10],1,sum)
    
  }else if(all(is.na(bt_temp[,12])==T)){
    bt_temp$recruits=apply(bt_temp[,6:11],1,sum)
  }
  
  bt_list[[i]]=bt_temp
}

sps_sub2=subset(sps2,Species!='Steelhead')
sps_sub2=subset(sps_sub2,Species!='steelhead')
steel.rm=sps_sub2[grepl('Steelhead',sps_sub2$Esu.Name),]
sps_sub2=sps_sub2[sps_sub2$Esu.Name %in% setdiff(sps_sub2$Esu.Name,steel.rm$Esu.Name),]
sps_sub2=subset(sps_sub2,Effective.Catch>0)



unique(paste(sps_sub2$Common.Population.Name,sps_sub2$Esu.Name))
sps_sub2$stock=paste(sps_sub2$Nwr.Population.Name,sps_sub2$Genus)

bt_list2=list()
nr=numeric()
for(i in 1:148){
  s=subset(sps_sub2,stock==levels(factor(sps_sub2$stock))[i])
  
  s=s[order(s$Brood.Year),]
  s=dplyr::distinct(s,Brood.Year,.keep_all=TRUE)
  s$run.size=s$Number.Of.Spawners*s$Fracwild+s$Effective.Catch
  s=s[is.na(s$Fracwild)==F,]
  s=s[complete.cases(s[,21:27]),]
  if(nrow(s)==0){next}
  
  bt_temp=data.frame(stock=unique(paste(s$Common.Population.Name,s$Species)),broodyear=s$Brood.Year,run=round(s$run.size),spawners=round(s$Number.Of.Spawners),recruits=NA,r1=NA,r2=NA,r3=NA,r4=NA,r5=NA,r6=NA,r7=NA,f.wild=s$Fracwild,catch=s$Effective.Catch)
  for(t in 1:nrow(s)){
    
    if(is.na(match(s$Brood.Year[t]-1,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-1,bt_temp$broodyear),6]=round(s$run.size[t]*s$Age.1.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-2,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-2,bt_temp$broodyear),7]=round(s$run.size[t]*s$Age.2.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-3,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-3,bt_temp$broodyear),8]=round(s$run.size[t]*s$Age.3.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-4,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-4,bt_temp$broodyear),9]=round(s$run.size[t]*s$Age.4.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-5,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-5,bt_temp$broodyear),10]=round(s$run.size[t]*s$Age.5.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-6,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-6,bt_temp$broodyear),11]=round(s$run.size[t]*s$Age.6.Returns[t])
    }
    if(is.na(match(s$Brood.Year[t]-7,bt_temp$broodyear))==F){
      bt_temp[match(s$Brood.Year[t]-7,bt_temp$broodyear),12]=round(s$run.size[t]*s$Age.7.Returns[t])
    }
    
  }
  if(all(na.omit(bt_temp[,11])==0)&all(na.omit(bt_temp[,12])==0)){
    bt_temp$recruits=apply(bt_temp[,6:10],1,sum)
    
  }else if(all(na.omit(bt_temp[,12])==0)){
    bt_temp$recruits=apply(bt_temp[,6:11],1,sum)
  }
  
  bt_temp=bt_temp[complete.cases(bt_temp$recruits),]
  
  bt_list2[[i]]=bt_temp
  nr[i]=nrow(bt_temp)
}
