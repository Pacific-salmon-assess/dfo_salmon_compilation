rm(list=ls())
##Salmon spawner-recruit time-series; dan.greenberg@dfo-mpo.gc.ca
library(here);library(dplyr);library(ggplot2);library(ggthemes);library(ggspatial);library(sf);library(rnaturalearth);library(rnaturalearthdata);library(wesanderson)
library(nlme); library(rlist)
#plot functions
source(here('code','functions.R'))

#Read datasets####
#sockeye
sockeye<- read.csv(here('data','raw data','sockeye','sockeye_data.csv'));sockeye_info<- read.csv(here('data','raw data','sockeye','sockeye_info.csv'));sockeye_source<- read.csv(here('data','raw data','sockeye','sockeye_sources.csv'))
psc_fraser_sockeye<- read.csv(here('data','raw data','sockeye','PSC_Fraser_broodtables.csv'))
skeena_nass_sockeye<- read.csv(here('data','raw data','sockeye','Skeena_Nass_sockeye.csv'))
bb_sockeye<- read.csv(here('data','raw data','sockeye','Bristol Bay Spawner-Recruit Data.csv'))
ogden_comp<-  read.csv(here('data','raw data','multispecies','Salmon_RS_Database.csv')); ogden_info<-read.csv(here('data','raw data','multispecies','Salmon_RS_Time_Series_Summary.csv'))
pse_comp<- read.csv(here('data','raw data','multispecies','PSE_RS.csv'));pse_dq=read.csv(here('data','raw data','multispecies','PSE_data_quality.csv'))
#chum
chum<- read.csv(here('data','raw data','chum','chum_data.csv'));chum_info<- read.csv(here('data','raw data','chum','chum_info.csv'));chum_source<- read.csv(here('data','raw data','chum','chum_sources.csv'))
chu_upd<- read.csv(here('data','raw data','chum','Chum S_R for BC_4.24.2023.csv'))
#pink
pink<- read.csv(here('data','raw data','pink','pink_data.csv'));pink_info<- read.csv(here('data','raw data','pink','pink_info.csv'));pink_source<- read.csv(here('data','raw data','pink','pink_sources.csv'))
pi_upd<- read.csv(here('data','raw data','pink','Pink S_R for BC_4.24.2023.csv'))
#coho
coho<- read.csv(here('data','raw data','coho','coho_data_fwmar.csv'));coho_info<- read.csv(here('data','raw data','coho','coho_info.csv'));coho_source<- read.csv(here('data','raw data','coho','coho_sources.csv'))
ifr_coho<- read.csv(here('data','raw data','coho','SR_IFC_BY_98-16_created_2021-07-19.csv'))
#chinook
chinook<- read.csv(here('data','raw data','chinook','chinook_data_totalage.csv'));chinook_info<- read.csv(here('data','raw data','chinook','chinook_info.csv'));chinook_source<- read.csv(here('data','raw data','chinook','chinook_sources.csv')) 
cc_comp<- read.csv(here('data','raw data','multispecies','AK-WCoast-Salmon-SR.csv'))
cow_chin<- read.csv(here('data','raw data','chinook','Cowichan_chinook_broodtable.csv'),na.strings = c('#N/A'))
harrison_chin<-read.csv(here('data','raw data','chinook','Harrison_chinook_broodtable.csv'))
shuswap_chin<-read.csv(here('data','raw data','chinook','Lower_Shuswap_chinook_broodtable.csv'))
nicola_chin<-read.csv(here('data','raw data','chinook','Nicola_chinook_broodtable.csv'))

#(partially) synonymize column names for sanity
colnames(sockeye)<- tolower(names(sockeye));colnames(sockeye_info)<- tolower(names(sockeye_info));colnames(sockeye_source)<- tolower(names(sockeye_source))
names(chum)<- gsub('.yr','year',names(chum)) #make the brood years equivalent
names(pink)<- gsub('.yr','year',names(pink))  #make the brood years equivalent
colnames(coho)<- tolower(names(coho));colnames(coho_info)<- tolower(names(coho_info));colnames(coho_source)<- tolower(names(coho_source));names(coho_source)[1]='source.id'
colnames(chinook)<- tolower(names(chinook));colnames(chinook_info)<- tolower(names(chinook_info));colnames(chinook_source)<- tolower(names(chinook_source))
colnames(ogden_comp)<- tolower(names(ogden_comp));colnames(ogden_info)<- tolower(names(ogden_info))
names(cc_comp)<- gsub('broodYr','broodyear',names(cc_comp));names(cc_comp)<- gsub('spawn','spawners',names(cc_comp));names(cc_comp)<- gsub('rec','recruits',names(cc_comp));colnames(cc_comp)[1]='stock.id'  #make the brood years equivalent
names(cow_chin)[1]='stock.id'
names(psc_fraser_sockeye)[1]='stock';names(psc_fraser_sockeye)[3]='broodyear'
names(skeena_nass_sockeye)=tolower(names(skeena_nass_sockeye));names(skeena_nass_sockeye)[5]='broodyear';names(skeena_nass_sockeye)[6]='spawners';names(skeena_nass_sockeye)[10]='recruits'
names(ifr_coho)[1]='stock';names(ifr_coho)[4]='spawners';names(ifr_coho)[5]='recruits'
names(harrison_chin)[1]='broodyear';names(harrison_chin)[4]='spawners';names(harrison_chin)[5]='recruits'
names(shuswap_chin)[1]='broodyear';names(shuswap_chin)[4]='spawners';names(shuswap_chin)[5]='recruits'
names(nicola_chin)[3]='broodyear';names(shuswap_chin)[4]='spawners';names(shuswap_chin)[5]='recruits'

#add ocean regions to info
chum_info$ocean.region=sockeye_info$ocean.region[match(chum_info$region,sockeye_info$region)]
chum_info$ocean.region[1:9]='WC'
pink_info$ocean.region=chum_info$ocean.region[match(pink_info$region,chum_info$region)]

#Sockeye####
stock_dat=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
#General stock characteristics, start and end of time-series, number of useable years (removing years with useflags),average spawner and recruit 

#From the top - sockeye compilation

#Merge in new PSC data for Fraser stocks and updated Bristol Bay data to existing compilation
fr_sock<- subset(sockeye, stock %in% psc_fraser_sockeye$stock)
bb_sock<<- subset(sockeye, stock %in% bb_sockeye$system)

sockeye2<- subset(sockeye, stock %notin% psc_fraser_sockeye$stock) #Drop out older data for Fraser R stocks
sockeye2<- subset(sockeye2, stock %notin% bb_sock$stock) #Drop out older data for Fraser R stocks

#Process the remaining stock data
sockeye_list=list()
for(i in 1:length(unique(sockeye2$stock.id))){
  s=subset(sockeye,stock.id==unique(sockeye2$stock.id)[i])
  s_info<- subset(sockeye_info,stock.id==unique(sockeye2$stock.id)[i])
  s_use=subset(s,useflag==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  
  stock_dat[i,1]=unique(s$stock.id)
  stock_dat[i,2]=unique(s$species)
  stock_dat[i,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat[i,4]=unique(s_info$lat)
  stock_dat[i,5]=unique(s_info$lon)
  stock_dat[i,6]=unique(s_info$ocean.region)
  stock_dat[i,7]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat[i,8]=min(s_use$broodyear)
    stock_dat[i,9]=max(s_use$broodyear)
    stock_dat[i,10]=length(s_use$broodyear)
    stock_dat[i,11]=max(s_use$spawners)
    stock_dat[i,12]=max(s_use$recruits)
  }else{
    stock_dat[i,8]=NA
    stock_dat[i,9]=NA
    stock_dat[i,10]=0
    stock_dat[i,11:12]=NA
  }
  
  stock_dat[i,13]=sockeye_source$source[match(s_info$source.id,sockeye_source$source.id)]
  stock_dat[i,15]=s_info$comment..we.will.update.this.later.
  
  sockeye_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
}

#Fraser sockeye stocks - PSC 2022 production dataset 
for(i in 1:length(unique(psc_fraser_sockeye$stock))){
  s=subset(psc_fraser_sockeye,production_stock_code==unique(psc_fraser_sockeye$production_stock_code)[i])
  s= s %>% mutate(recruits = rowSums(s[,9:21],na.rm = TRUE))
  s$species<- rep('Sockeye',nrow(s))
  #to determine whether to use effective female spawners or total spawners based on data availability
  if(length(na.omit(s$total_broodyr_EFS))<length(na.omit(s$total_broodyr_spawners))){
   names(s)[4]='spawners' 
  }
  names(s)[5]='spawners' #use effective female spawners for these stocks
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)

  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=sockeye_info$lat[2] #lat for mouth of Fraser
  stock_dat_temp[,5]=sockeye_info$lon[2] #lon for mouth of Fraser
  stock_dat_temp[,6]='WC' #West Coast
  stock_dat_temp[,7]='BC' #British Columbia
  
  stock_dat_temp[,8]=min(s$broodyear)
  stock_dat_temp[,9]=max(s$broodyear)
  stock_dat_temp[,10]=length(s$broodyear)
  stock_dat_temp[,11]=max(s$spawners)
  stock_dat_temp[,12]=max(s$recruits)
  stock_dat_temp[,13]='Eric Taylor, Pacific Salmon Commission, 2022'
  stock_dat_temp[,15]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

#Skeena-Nass sockeye
skeena_nass_sockeye_4k=subset(skeena_nass_sockeye,label=='Filter45') #Use data without infill, but filtered out productivity (R/S) estimates over 45
skeena_nass_sockeye_4k=skeena_nass_sockeye_4k[complete.cases(skeena_nass_sockeye_4k$stock),]
skeena_nass_sockeye_4k=subset(skeena_nass_sockeye_4k,is.na(stock)==F)
skeena_nass_sockeye_4k=subset(skeena_nass_sockeye_4k,stock!='Meziadin') #longer time-series in other dataset, so removing this
skeena_nass_sockeye_4k$stock=gsub('Bear','Bear-Skeena',skeena_nass_sockeye_4k$stock) #renaming Bear stock to avoid conflict with Bear R. Alaska
for(i in 1:length(unique(skeena_nass_sockeye_4k$stock))){
  s=subset(skeena_nass_sockeye_4k,stock==unique(skeena_nass_sockeye_4k$stock)[i])
  s= s[complete.cases(s$spawners),];s=s[complete.cases(s$recruits),] #keep years with both spawner & recruit estimates
  if(nrow(s)==0){next}
  s$species<- rep('Sockeye',nrow(s))
 
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock)
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  if(s$stock[1]=='Lower Nass Sea & River Type'|s$stock[1]=='Meziadin'|s$stock[1]=='Kwinageese'|s$stock[1]=='Damdochax'|s$stock[1]=='Upper Nass River Type'){ #Nass stocks
    stock_dat_temp[,4]=54.9898 #lat need to do these (one for Skeena estuary and one for Nass)
    stock_dat_temp[,5]=-130.02 #lon 
    
  }else{
    stock_dat_temp[,4]=54.2237 #lat need to do these (one for Skeena estuary and one for Nass)
    stock_dat_temp[,5]=-129.831 #lon 
  }
  stock_dat_temp[,6]='WC'
  stock_dat_temp[,7]='BC'
  
  stock_dat_temp[,8]=min(s$broodyear)
  stock_dat_temp[,9]=max(s$broodyear)
  stock_dat_temp[,10]=length(s$broodyear)
  stock_dat_temp[,11]=max(s$spawners)
  stock_dat_temp[,12]=max(s$recruits)
  stock_dat_temp[,13]='Charmaine Carr-Harris, DFO, 2022'
  stock_dat_temp[,15]='Used filtered data - R/S >45 were removed'
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

#Bristol bay

bb_info<- subset(sockeye_info,stock %in% bb_sockeye$system)

for(i in 1:length(unique(bb_sockeye$system))){
  s=subset(bb_sockeye,system==unique(bb_sockeye$system)[i])
  s=s[complete.cases(s$rps),]
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$system),'Sockeye',sep='-')
  stock_dat_temp[,4]=bb_info$lat[i] #lat 
  stock_dat_temp[,5]=bb_info$lon[i] #lon
  stock_dat_temp[,6]='BS' #Bering Sea
  stock_dat_temp[,7]='AK' #Alaska
  
  stock_dat_temp[,8]=min(s$broodYr)
  stock_dat_temp[,9]=max(s$broodYr)
  stock_dat_temp[,10]=length(s$broodYr)
  stock_dat_temp[,11]=max(s$spawn)
  stock_dat_temp[,12]=max(s$rec)
  stock_dat_temp[,13]='Curry Cunningham, U. of Alaska Fairbanks, 2023'
  stock_dat_temp[,15]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  names(s)=c('stock','broodyear','spawners','recruits','rps','rec_part','rps_part')
  s$species='Sockeye'
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

sockeye_filtered<- do.call("rbind", sockeye_list)

#Chum####
#Merge in updated WA pink series
chu_upd$stock=gsub("\\s*\\([^\\)]+\\)",'',chu_upd$stock) #to make names synonymous bwn datasets remove parentheses on stock names
chu_old<- subset(chum, stock %in% chu_upd$stock)
length(unique(chu_old$stock));length(unique(chu_upd$stock)) #1 new stock, 9 updated
names(chu_upd)[1]='stock.id'
chu_upd$stock.id=ifelse(chu_upd$stock.id==310,max(chum$stock.id)+1,chu_upd$stock.id) #remove duplicate stock ids, make new one for extra stock

#add info
chum_info[nrow(chum_info)+1,1:9]=c(max(chu_upd$stock.id),'Chum',unique(chu_upd$stock)[10],'Inside WA','Inside WA','WA',48.1618, -123.59,23)

chum_source[23,1:2]=c(23,'Marisa Litz, WDFW, April 2023')
chum_info$source.id[1:9]=23
chum_info$ocean.region[nrow(chum_info)]='WC'

chum2<- subset(chum, stock %notin% chu_upd$stock) #Drop out older data for Fraser R stocks
chu_upd2=chu_upd[,1:9];
chu_upd2$recruits.2=NA;chu_upd2[,11:13]=chu_upd[,10:12];chu_upd2$recruits.6=NA;chu_upd2$recruits.7=NA
chu_upd2$age=chu_upd$age
names(chu_upd2)[6]='broodyear'

chum2<- rbind(chu_upd2,chum2)
length(unique(chum2$stock))

chum_list=list()
for(i in 1:length(unique(chum2$stock.id))){
  s=subset(chum2,stock.id==unique(chum2$stock.id)[i])
  s_info<- subset(chum_info,stock.id==unique(chum2$stock.id)[i])
  s_use=subset(s,use==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock.id)
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$ocean.region)
  stock_dat_temp[,7]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,8]=min(s_use$broodyear)
    stock_dat_temp[,9]=max(s_use$broodyear)
    stock_dat_temp[,10]=length(s_use$broodyear)
    stock_dat_temp[,11]=max(s_use$spawners)
    stock_dat_temp[,12]=max(s_use$recruits)
    }else{
    stock_dat_temp[,8]=NA
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=0
    stock_dat_temp[,11:12]=NA
  }
 
  s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
  if(length(s.id)==1){ stock_dat_temp[,13]=chum_source$source[match(s_info$source.id,sockeye_source$source.id)]
}
  if(length(s.id)==2){
    source<- subset(chum_source, source.id %in% s.id)
    stock_dat_temp[,13]=paste(source$source[1],source$source[2],sep='; ')
  }
  stock_dat_temp[,15]=s_info$comment
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chum_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
}
chum_filtered<- do.call("rbind", chum_list)

#Pink####
#Merge in updated WA pink series
pi_upd=pi_upd[complete.cases(pi_upd$use),]
pi_upd$stock=gsub("\\s*\\([^\\)]+\\)",'',pi_upd$stock) #to make names synonymous bwn datasets remove parentheses on stock names
pi_old<- subset(pink, stock %in% pi_upd$stock)
length(unique(pi_old$stock));length(unique(pi_upd$stock)) #2 new stocks, 7 updated
names(pi_upd)[1]='stock.id'
pi_upd$stock.id=ifelse(pi_upd$stock.id==208,max(pink$stock.id)+1,pi_upd$stock.id) #remove duplicate stock ids, make new one for extra stock
pi_upd$stock.id=ifelse(pi_upd$stock.id==209,max(pink$stock.id)+2,pi_upd$stock.id)
#add info
pink_info[nrow(pink_info)+1,1:9]=c(max(pi_upd$stock.id)-1,'Pink',unique(pi_upd$stock)[8],'Inside WA','Inside WA','WA',47.101, -122.706,7)
pink_info[nrow(pink_info)+1,1:9]=c(max(pi_upd$stock.id),'Pink',unique(pi_upd$stock)[9],'Inside WA','Inside WA','WA',447.16, -122.91,7)
pink_info$ocean.region[c(nrow(pink_info)-1,nrow(pink_info))]='WC'
pink_source[7,1:2]=c(7,'Marisa Litz, WDFW, April 2023')
pink_info$source.id[1:7]=7

pink2<- subset(pink, stock %notin% pi_upd$stock) #Drop out older data for Fraser R stocks
pi_upd2=pi_upd[,1:9]
names(pi_upd2)[6]='broodyear'

pink2<- rbind(pi_upd2,pink2)
length(unique(pink2$stock))

pink_list=list()
for(i in 1:length(unique(pink2$stock.id))){
  s=subset(pink2,stock.id==unique(pink2$stock.id)[i])
  s_info<- subset(pink_info,stock.id==unique(pink2$stock.id)[i])
  s_use=subset(s,use==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  odd.yrs=ifelse(s_use$broodyear %% 2 == 0,0,1)
  s_use$odd=odd.yrs
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  if(length(levels(factor(odd.yrs)))>1){
    s_even=subset(s_use,odd==0)
    s_odd=subset(s_use,odd==1)
    s_even$species='Pink-Even'
    s_odd$species='Pink-Odd'
    
    stock_dat_temp[1:2,1]=unique(s$stock.id)
    stock_dat_temp[1,2]='Pink-Even'
    stock_dat_temp[2,2]='Pink-Odd'
    stock_dat_temp[1,3]=paste(unique(s$stock),'Pink-Even',sep='-')
    stock_dat_temp[2,3]=paste(unique(s$stock),'Pink-Odd',sep='-')
    stock_dat_temp[,4]=unique(s_info$lat)
    stock_dat_temp[,5]=unique(s_info$lon)
    stock_dat_temp[,6]=unique(s_info$ocean.region)
    stock_dat_temp[,7]=unique(s_info$jurisdiction)
    
    if(nrow(s_use)!=0){
      stock_dat_temp[1,8]=min(s_even$broodyear)
      stock_dat_temp[1,9]=max(s_even$broodyear)
      stock_dat_temp[1,10]=length(s_even$broodyear)
      stock_dat_temp[1,11]=max(s_even$spawners)
      stock_dat_temp[1,12]=max(s_even$recruits)
      stock_dat_temp[2,8]=min(s_odd$broodyear)
      stock_dat_temp[2,9]=max(s_odd$broodyear)
      stock_dat_temp[2,10]=length(s_odd$broodyear)
      stock_dat_temp[2,11]=max(s_odd$spawners)
      stock_dat_temp[2,12]=max(s_odd$recruits)
    }else{
      stock_dat_temp[,8]=NA
      stock_dat_temp[,9]=NA
      stock_dat_temp[,10]=0
      stock_dat_temp[,11:12]=NA
    }
    
    s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
    if(length(s.id)==1){stock_dat_temp[,13]=pink_source$source[match(s_info$source.id,sockeye_source$source.id)]
    }
    if(length(s.id)==2){
      source<- subset(pink_source, source.id %in% s.id)
      stock_dat_temp[,13]=paste(source$source[1],source$source[2],sep='; ')
    }
    stock_dat_temp[,15]=s_info$comment
    
    stock_dat=rbind(stock_dat,stock_dat_temp)
    
    pink_list[[i]]=rbind(s_even[,c('stock','species','broodyear','recruits','spawners')],s_odd[,c('stock','species','broodyear','recruits','spawners')])
  }else{
    s_use$species=ifelse(all(odd.yrs==0)==T,'Pink-Even','Pink-Odd')
    
    stock_dat_temp[,1]=unique(s_use$stock.id)
    stock_dat_temp[,2]=unique(s_use$species)
    stock_dat_temp[,3]=paste(unique(s_use$stock),unique(s_use$species),sep='-')
    stock_dat_temp[,4]=unique(s_info$lat)
    stock_dat_temp[,5]=unique(s_info$lon)
    stock_dat_temp[,6]=unique(s_info$ocean.region)
    stock_dat_temp[,7]=unique(s_info$jurisdiction)
    
    if(nrow(s_use)!=0){
      stock_dat_temp[,8]=min(s_use$broodyear)
      stock_dat_temp[,9]=max(s_use$broodyear)
      stock_dat_temp[,10]=length(s_use$broodyear)
      stock_dat_temp[,11]=max(s_use$spawners)
      stock_dat_temp[,12]=max(s_use$recruits)
    }else{
      stock_dat_temp[,8]=NA
      stock_dat_temp[,9]=NA
      stock_dat_temp[,10]=0
      stock_dat_temp[,11:12]=NA
    }
    
    s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
    if(length(s.id)==1){stock_dat_temp[,13]=pink_source$source[match(s_info$source.id,sockeye_source$source.id)]
    }
    if(length(s.id)==2){
      source<- subset(pink_source, source.id %in% s.id)
      stock_dat_temp[,13]=paste(source$source[1],source$source[2],sep='; ')
    }
    stock_dat_temp[,15]=s_info$comment
    
    stock_dat=rbind(stock_dat,stock_dat_temp)
    
    pink_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
  }
}
pink_filtered<- do.call("rbind", pink_list)

#Chinook####
#Add in Cowichan chinook
chinook=rbind(chinook,cow_chin) #add in S-R data
chinook_info[21,1:7]=c(333,'Chinook','Cowichan','WC','Vancouver Island','Vancouver Island','BC');chinook_info$lat[21]=48.7581;chinook_info$lon[21]=-123.6242;chinook_info$source.id[21]=10 #add in metadata

chinook_info[22,1:7]=c(334,'Chinook','Harrison','WC','Fraser River','Vancouver Island','BC');chinook_info$lat[22]=sockeye_info$lat[2];chinook_info$lon[22]=sockeye_info$lon[2];chinook_info$source.id[22]=11 #add in metadata
chinook_info[23,1:7]=c(335,'Chinook','Lower Shuswap','WC','Fraser River','Fraser River','BC');chinook_info$lat[23]=sockeye_info$lat[2];chinook_info$lon[23]=sockeye_info$lon[2];chinook_info$source.id[23]=11 #add in metadata
chinook_info[24,1:7]=c(336,'Chinook','Nicola','WC','Fraser River','Fraser River','BC');chinook_info$lat[24]=sockeye_info$lat[2];chinook_info$lon[24]=sockeye_info$lon[2];chinook_info$source.id[24]=12 #add in metadata
chinook_source[10,1]=10;chinook_source$title[10]='Karalea Cantera, DFO, 2022' #add in source
chinook_source[11,1]=11;chinook_source$title[11]='Chucken Parken, DFO, 2022' #add in source
chinook_source[12,1]=12;chinook_source$title[12]='Luke Warkentin, DFO, 2022' #add in source

chinook_list=list()
for(i in 1:length(unique(chinook$stock.id))){
  s=subset(chinook,stock.id==unique(chinook$stock.id)[i])
  s_info<- subset(chinook_info,stock.id==unique(chinook$stock.id)[i])
  s_use=subset(s,useflag==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock.id)
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$ocean.region)
  stock_dat_temp[,7]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,8]=min(s_use$broodyear)
    stock_dat_temp[,9]=max(s_use$broodyear)
    stock_dat_temp[,10]=length(s_use$broodyear)
    stock_dat_temp[,11]=max(s_use$spawners)
    stock_dat_temp[,12]=max(s_use$recruits)
  }else{
    stock_dat_temp[,8]=NA
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=0
    stock_dat_temp[,11:12]=NA
  }
  
  
  s.id=as.numeric(strsplit(as.character(s_info$source.id),',')[[1]])
  if(length(s.id)==1){
    stock_dat_temp[,13]=chinook_source$title[match(s_info$source.id,sockeye_source$source.id)]
    stock_dat_temp[,14]=chinook_source$url[match(s_info$source.id,sockeye_source$source.id)]
    
  }
  if(length(s.id)==2){
    source<- subset(chinook_source, source.id %in% s.id)
    stock_dat_temp[,13]=paste(source$title[1],source$title[2],sep='; ')
    stock_dat_temp[,14]=paste(source$url[1],source$url[2],sep='; ')
  }
  stock_dat_temp[,15]=NA #no comments
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chinook_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
}

#Harrison chinook
harrison_chin3=subset(harrison_chin,is.na(recruits)==F&is.na(spawners)==F) #remove years without recruit data

row.n=nrow(stock_dat)+1
stock_dat[row.n,1]=NA #no pre-assigned stock id
stock_dat[row.n,2]=chinook_info[22,2]
stock_dat[row.n,3]=paste(chinook_info[22,3],chinook_info[22,2],sep='-')
stock_dat[row.n,4]=chinook_info[22,8]
stock_dat[row.n,5]=chinook_info[22,9]
stock_dat[row.n,6]='WC'
stock_dat[row.n,7]='BC'
stock_dat[row.n,8]=min(harrison_chin3$broodyear)
stock_dat[row.n,9]=max(harrison_chin3$broodyear)
stock_dat[row.n,10]=length(harrison_chin3$broodyear)
stock_dat[row.n,11]=max(harrison_chin3$spawners)
stock_dat[row.n,12]=max(harrison_chin3$recruits)
stock_dat[row.n,13]=chinook_source$title[chinook_info$source.id[22]]
stock_dat[row.n,14]='NA'
stock_dat[row.n,15]='NA'

harrison_chin3$stock=rep('Harrison',nrow(harrison_chin3))
harrison_chin3$species=rep('Chinook',nrow(harrison_chin3))

chinook_list[[22]]=harrison_chin3[,c('stock','species','broodyear','recruits','spawners')]

#Lower Shuswap chinook
shuswap_chin$stock=rep('Lower Shuswap',nrow(shuswap_chin))
shuswap_chin$species=rep('Chinook',nrow(shuswap_chin))
shuswap_chin=subset(shuswap_chin,is.na(recruits)==F&is.na(spawners)==F) #remove years without recruit data

row.n=nrow(stock_dat)+1
stock_dat[row.n,1]=NA #no pre-assigned stock id
stock_dat[row.n,2]=chinook_info[23,2]
stock_dat[row.n,3]=paste(chinook_info[23,3],chinook_info[23,2],sep='-')
stock_dat[row.n,4]=chinook_info[23,8]
stock_dat[row.n,5]=chinook_info[23,9]
stock_dat[row.n,6]='WC'
stock_dat[row.n,7]='BC'
stock_dat[row.n,8]=min(shuswap_chin$broodyear)
stock_dat[row.n,9]=max(shuswap_chin$broodyear)
stock_dat[row.n,10]=length(shuswap_chin$broodyear)
stock_dat[row.n,11]=max(shuswap_chin$spawners)
stock_dat[row.n,12]=max(shuswap_chin$recruits)
stock_dat[row.n,13]=chinook_source$title[chinook_info$source.id[23]]

shuswap_chin$stock=rep('Lower Shuswap',nrow(shuswap_chin))
shuswap_chin$species=rep('Chinook',nrow(shuswap_chin))
shuswap_chin=subset(shuswap_chin,is.na(recruits)==F&is.na(spawners)==F) #remove years without recruit data


chinook_list[[23]]=shuswap_chin[,c('stock','species','broodyear','recruits','spawners')]

#Nicola chinook
#use total spawners as per Warkentin et al. 2020
nicola_recruits=nicola_chin %>% group_by(broodyear) %>% summarize(recruits=sum(recruits),spawners=sum(total_spawners))
#follow Warkentin et al. 2020 and use cohorts from 1992 to 2013
nicola_recruits=subset(nicola_recruits,broodyear>1991)
nicola_recruits$stock=rep('Nicola',nrow(nicola_recruits))
nicola_recruits$species=rep('Chinook',nrow(nicola_recruits))

row.n=nrow(stock_dat)+1
stock_dat[row.n,1]=NA #no pre-assigned stock id
stock_dat[row.n,2]=chinook_info[24,2]
stock_dat[row.n,3]=paste(chinook_info[24,3],chinook_info[24,2],sep='-')
stock_dat[row.n,4]=chinook_info[24,8]
stock_dat[row.n,5]=chinook_info[24,9]
stock_dat[row.n,6]='WC'
stock_dat[row.n,7]='BC'
stock_dat[row.n,8]=min(nicola_recruits$broodyear)
stock_dat[row.n,9]=max(nicola_recruits$broodyear)
stock_dat[row.n,10]=length(nicola_recruits$broodyear)
stock_dat[row.n,11]=max(nicola_recruits$spawners)
stock_dat[row.n,12]=max(nicola_recruits$recruits)
stock_dat[row.n,13]=chinook_source$title[chinook_info$source.id[24]]

chinook_list[[24]]=nicola_recruits[,c('stock','species','broodyear','recruits','spawners')]

chinook_filtered<- do.call("rbind", chinook_list)

#Coho####

coho_list=list()
for(i in 1:length(unique(coho$stock.id))){
  s=subset(coho,stock.id==unique(coho$stock.id)[i])
  #Some stocks from have repeated time-series with multiple estimates of escapement based extrapolations of true escapement from peak abundance estimated from aerial surveys. These are eg. 25%, 50%, 100% of true escapement.
  #Just taking a single time-series for these - the particular estimate does not matter for our purposes in comparing stationary and time-varying stock-recruitment dynamics
  if(any(duplicated(s$broodyear))==T){
    s=distinct(s,broodyear,.keep_all = T)
  }
  s_info<- subset(coho_info,stock.id==unique(coho$stock.id)[i])
  s_use=subset(s,useflag==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock.id)
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$ocean.region)
  stock_dat_temp[,7]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,8]=min(s_use$broodyear)
    stock_dat_temp[,9]=max(s_use$broodyear)
    stock_dat_temp[,10]=length(s_use$broodyear)
    stock_dat_temp[,11]=max(s_use$spawners)
    stock_dat_temp[,12]=max(s_use$recruits)
  }else{
    stock_dat_temp[,8]=NA
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=0
    stock_dat_temp[,11:12]=NA
  }
  
  s.id=as.numeric(strsplit(as.character(s_info$source.id),',')[[1]])
  if(length(s.id)==1){
    stock_dat_temp[,13]=coho_source$title[match(s_info$source.id,sockeye_source$source.id)]
    stock_dat_temp[,14]=coho_source$url[match(s_info$source.id,sockeye_source$source.id)]
  }
  if(length(s.id)==2){
    source<- subset(coho_source, source.id %in% s.id)
    stock_dat_temp[,13]=paste(source$source[1],source$source[2],sep='; ')
    stock_dat_temp[,14]=paste(source$url[1],source$url[2],sep='; ')
  }
  stock_dat_temp[,15]=NA #no comments
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  coho_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
}

#Interior Fraser coho brood tables
for(i in 1:length(unique(ifr_coho$stock))){
  s=subset(ifr_coho,stock==unique(ifr_coho$stock)[i])
  s$species=rep('Coho',nrow(s))
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock)
  stock_dat_temp[,2]='Coho'
  stock_dat_temp[,3]=paste(unique(s$stock),'Coho',sep='-')
  stock_dat_temp[,4]=sockeye_info$lat[2] #lat for mouth of Fraser
  stock_dat_temp[,5]=sockeye_info$lon[2] #lon for mouth of Fraser
  stock_dat_temp[,6]='WC'
  stock_dat_temp[,7]='BC'
  stock_dat_temp[,8]=min(s$broodyear)
  stock_dat_temp[,9]=max(s$broodyear)
  stock_dat_temp[,10]=length(s$broodyear)
  stock_dat_temp[,11]=max(s$spawners)
  stock_dat_temp[,12]=max(s$recruits)
  stock_dat_temp[,13]='Michael Arbeider, DFO, 2022'
  stock_dat_temp[,14]=NA #no comments

  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  coho_list[[length(unique(coho$stock.id))+i]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

coho_filtered<- do.call("rbind", coho_list)

##Issue 1: many redundant stocks across datasets - first pass to filter these out
###Curry Cunningham compilation
cc_comp=cc_comp[complete.cases(cc_comp$recruits),]
cc_comp$stock.name<- paste(cc_comp$stock,cc_comp$species,sep='-')
#All pink/sockeye/chum stocks are the same as in the existing dataset - but some have slightly different names due to abbreviations etc
cc_comp<- subset(cc_comp, species %in% c('Chinook','Coho'))
cc_comp2<- subset(cc_comp, stock.name %notin% stock_dat$stock.name)

unique(cc_comp2$stock.name)

#Approximate lat/lons for these stocks taken from google maps - at mouth of the river system
alsek_klukshu=cbind(59.1348,-138.6067)
columbia=cbind(46.2508,-124.0143) #for all stocks in Columbia river system (Columbia, snake river, williamette)
oregon_coast=cbind(44.6175, -124.1093) #ESU for chinook south of Columbia/north of Cape Blanco - location from Newport (approx. mid range)
#puget sound stock lat/lons:
puget_sound_lat_lons<- data.frame(river.system=NA,lat=NA,lon=NA)
puget_sound_lat_lons[1,]=cbind('Green River',47.5925,-122.3600) #Green River lat/lon
puget_sound_lat_lons[2,]=cbind('Cedar River',47.5039,-122.2169) #Cedar River lat/lon
puget_sound_lat_lons[3,]=cbind('Skagit River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
puget_sound_lat_lons[4,]=cbind('Stillaguamish River',48.2465,-122.3957) #Stillaguimish river
puget_sound_lat_lons[5,]=cbind('Nisqually River',47.0996,-122.7029) #Nisqually river
puget_sound_lat_lons[6,]=cbind('Puyallup River',47.2472,-122.4289) #Puyallup river & white river tributary
puget_sound_lat_lons[7,]=cbind('Skokomish River',47.3436,-123.1212) #Skokomish river & white river tributary
puget_sound_lat_lons[8,]=cbind('Snohomisin River',48.0206,-122.2122) #Tributaries of Snohomisin (Snoqualmie & Skykomish)
puget_sound_lat_lons[9,]=cbind('Mid-Hood Canal',47.6393,-122.9299) #mid hood canal - diswallups/duckabush/hamma hamma watersheds -coordinate is at mouth of the Duckabush system (mid way)

#putting in lat/lon info for these stocks manually:
cc_info<- distinct(cc_comp2,stock.name,.keep_all=T)
cc_info$lat=NA;cc_info$lon=NA
cc_info[1,12:13]=alsek_klukshu #Alsek-Chinook
cc_info[2:28,12]=columbia[,1];cc_info[2:28,13]=columbia[,2] #Columbia river chinook (26 pop)
cc_info[29,12:13]=puget_sound_lat_lons[2,2:3] #Cedar river chinook
cc_info[30,12:13]=puget_sound_lat_lons[1,2:3] #Green river
cc_info[31,12:13]=puget_sound_lat_lons[3,2:3] #Sauk river
cc_info[32,12:13]=puget_sound_lat_lons[3,2:3] #skagit river
cc_info[33,12:13]=puget_sound_lat_lons[9,2:3] #mid-hood canal
cc_info[34,12:13]=puget_sound_lat_lons[5,2:3] #Nisqually river
cc_info[35,12:13]=puget_sound_lat_lons[4,2:3] #Stillaguimish river
cc_info[36,12:13]=puget_sound_lat_lons[6,2:3] #Puyallup river
cc_info[37,12:13]=puget_sound_lat_lons[7,2:3] #Skokomish river
cc_info[38,12:13]=puget_sound_lat_lons[8,2:3] #Skykomish River
cc_info[39,12:13]=puget_sound_lat_lons[8,2:3] # Snoqualmie River
cc_info[40,12:13]=puget_sound_lat_lons[4,2:3] #Stillaguimish river
cc_info[41,12:13]=puget_sound_lat_lons[3,2:3] #Suiattle River
cc_info[42,12:13]=puget_sound_lat_lons[3,2:3] #Upper Sauk River
cc_info[43,12:13]=puget_sound_lat_lons[3,2:3] #Skagit River
cc_info[44,12:13]=puget_sound_lat_lons[6,2:3] #White River
cc_info[45:58,12]=columbia[,1];cc_info[45:58,13]=columbia[,2] #Willamette (Columbia)
cc_info[59:60,12]=oregon_coast[,1];cc_info[59:60,13]=oregon_coast[,2] #Oregon coast coho
cc_info[61:62,12]=columbia[,1];cc_info[61:62,13]=columbia[,2] #Willamette (Columbia) coho

#add in these new stocks
cc_comp_list=list()
for(i in 1:length(unique(cc_comp2$stock.name))){
  s=subset(cc_comp2,stock.name==unique(cc_comp2$stock.name)[i])
  s_info=subset(cc_info,stock.name==unique(cc_comp2$stock.name)[i])
  #Some stocks from have repeated time-series with multiple estimates of escapement based extrapolations of true escapement from peak abundance estimated from aerial surveys. These are eg. 25%, 50%, 100% of true escapement.
  #Just taking a single time-series for these - the particular estimate does not matter for our purposes in comparing stationary and time-varying stock-recruitment dynamics
  if(any(duplicated(s$broodyear))==T){
    s=distinct(s,broodyear,.keep_all = T)
  }
  s_use=subset(s,is.na(spawners)==F&is.na(recruits)==F)
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,max.spawners=NA,max.recruits=NA,source=NA,url=NA,comments=NA)
  
  stock_dat_temp[,1]=unique(s$stock.id)
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$large.region)
  if(s_info$region=='Southeast'){stock_dat_temp[,7]='AK'}
  if(s_info$region=='Interior Columbia'|s_info$region=='Puget Sound'){stock_dat_temp[,7]='WA'}
  if(s_info$region=='Willamette/Lower Columbia'|s_info$region=='Oregon Coast'){stock_dat_temp[,7]='OR'}
  if(nrow(s_use)!=0){
    stock_dat_temp[,8]=min(s_use$broodyear)
    stock_dat_temp[,9]=max(s_use$broodyear)
    stock_dat_temp[,10]=length(s_use$broodyear)
    stock_dat_temp[,11]=max(s_use$spawners)
    stock_dat_temp[,12]=max(s_use$recruits)
  }else{
    stock_dat_temp[,8]=NA
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=0
    stock_dat_temp[,11:12]=NA
  }
  stock_dat_temp[,13]='Curry Cunningham/Brian Burke, 2022' #source to be confirmed with Curry
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  cc_comp_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
}
cc_comp_filtered<- do.call("rbind", cc_comp_list)


#Print out data####
filtered_productivity_data=rbind(sockeye_filtered,chum_filtered,pink_filtered,coho_filtered,chinook_filtered,cc_comp_filtered)

#Stock overview
stock_dat=subset(stock_dat,n.years>0)
stock_dat$stock.id=seq(1:nrow(stock_dat))
filtered_productivity_data$stock=paste(filtered_productivity_data$stock,filtered_productivity_data$species,sep='-')
filtered_productivity_data$stock.id=stock_dat$stock.id[match(filtered_productivity_data$stock,stock_dat$stock.name)]

#Write datasets
write.csv(filtered_productivity_data,here('data','filtered datasets',paste('salmon_productivity_compilation',Sys.Date(),'.csv',sep='')))
write.csv(stock_dat,here('data','filtered datasets',paste('stock_info',Sys.Date(),'.csv',sep='')))
