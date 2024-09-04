rm(list=ls())
##Salmon spawner-recruit time-series; dan.greenberg@dfo-mpo.gc.ca
library(here);library(dplyr); library(tidyverse)
#plot functions
source(here('code','functions.R'))

#Read datasets####
#sockeye
sockeye<- read.csv(here('data','raw data','sockeye','sockeye_data.csv'));sockeye_info<- read.csv(here('data','raw data','sockeye','sockeye_info.csv'));sockeye_source<- read.csv(here('data','raw data','sockeye','sockeye_sources.csv'))
psc_fraser_sockeye<- read.csv(here('data','raw data','sockeye','PSC_Fraser_broodtables.csv'))
skeena_sockeye<- read.csv(here('data','raw data','sockeye','skeena_sockeye_broodtable.csv'))
nass_sockeye<- read.csv(here('data','raw data','sockeye','nass_sockeye_broodtable.csv'))
#bb_sockeye<- read.csv(here('data','raw data','sockeye','Bristol Bay Spawner-Recruit Data.csv'))
somass_soc<- read.csv(here('data','raw data','sockeye','Somass_stock_recruit_1977-2022.csv'))
goodnews_soc<- read.csv(here('data','raw data','sockeye','Goodnews_Sockeye_Brood_Table3.2.2023.csv'))
bb_soc<- read.csv(here('data','raw data','sockeye','BBay_Brood_Tables_2023.csv'))
kasilof_soc<- read.csv(here('data','raw data','sockeye','Kasilof_Brood_Table_2023.csv'))
kenai_soc<- read.csv(here('data','raw data','sockeye','Kenai_Brood_Table_2023.csv'))
mfgn_soc<- read.csv(here('data','raw data','sockeye','MFGN Sockeye Brood Table_2023.csv'))
seak_soc<- read.csv(here('data','raw data','sockeye','SEAK Sockeye Data_2023.csv'))
ww_soc<- read.csv(here('data','raw data','sockeye','Westward Brood tables_2023.csv'))


###
pse_ncc <- read.csv(here('data','raw data','multispecies','CC_age_2023_03_03.csv')) #updated PSE north and central coast data 
pse_all <- read.csv(here('data','raw data','multispecies','PSE_all_spp_RS.2023-Sep-20.csv')) #updated PSE data 
pse_ncc$Total=ifelse(pse_ncc$Total==0,NA,pse_ncc$Total)
pse_ncc$Escape=ifelse(pse_ncc$Escape==0,NA,pse_ncc$Escape)
pse_all$recruits=ifelse(pse_all$recruits==0,NA,pse_all$recruits)
pse_all$spawners=ifelse(pse_all$spawners==0,NA,pse_all$spawners)



#pacific salmon explorer
pse=read.csv(here('data','raw data','multispecies','dataset5.csv'))
dqpse=read.csv(here('data','raw data','multispecies','dataset390.csv'))
dqpse2=tidyr::spread(dqpse,key=parameter,value=datavalue)
hdq=subset(dqpse2,dq_score>8) #minimum total data quality score of 9 - can include 'unknown' DQ records

pse_chum=subset(hdq,species=='Chum')
pse_chin=subset(hdq,species=='Chinook')
pse_chin=subset(pse_chin,regionname!='Skeena') #have alternative skeena datasets for chinook
pse_coho=subset(hdq,species=='Coho')
pse_soc=subset(hdq,species=='Lake sockeye'|species=='River sockeye')
pse_soc=subset(pse_soc,regionname %notin% c('Skeena','Fraser')) #have alternative skeena/fraser datasets for sockeye

pse_pink=subset(hdq,species=='Pink (even)'|species=='Pink (odd)')
hdq2=rbind(pse_chum,pse_chin,pse_coho,pse_pink,pse_soc)

pse=subset(pse,spawners>1&recruits>1)
pse_df=subset(pse,cuid %in% hdq2$cuid)
pse_df$stock=paste(pse_df$species_name,pse_df$cu_name_pse)
hdq2$stock=paste(hdq2$species,hdq2$location)

pse_sum=pse_df%>%group_by(stock) %>% summarize(n=n()) %>%subset(n>14)
pse_df=pse_df[pse_df$stock%in%pse_sum$stock,] 
hdq3=subset(hdq2,stock %in% pse_df$stock)

#WA chum
chum<- read.csv(here('data','raw data','chum','chum_data.csv'));chum_info<- read.csv(here('data','raw data','chum','chum_info.csv'));chum_source<- read.csv(here('data','raw data','chum','chum_sources.csv'))
chu_upd<- read.csv(here('data','raw data','chum','Chum S_R for BC_4.24.2023.csv'))
#WA pink
pink<- read.csv(here('data','raw data','pink','pink_data_update.Sep20203.csv'));pink_info<- read.csv(here('data','raw data','pink','pink_info.csv'));pink_source<- read.csv(here('data','raw data','pink','pink_sources.csv'))
pi_upd<- read.csv(here('data','raw data','pink','Pink S_R for BC_4.24.2023.csv'))

#coho
coho<- read.csv(here('data','raw data','coho','coho_data_fwmar.csv'));coho_info<- read.csv(here('data','raw data','coho','coho_info.csv'));coho_source<- read.csv(here('data','raw data','coho','coho_sources.csv'))
ifr_coho<- read.csv(here('data','raw data','coho','SR_IFC_BY_98-16_created_2021-07-19.csv'))
names(ifr_coho)[6:7]=c('r3','r4')

#chinook
chinook<- read.csv(here('data','raw data','chinook','chinook_data_totalage.csv'));chinook_info<- read.csv(here('data','raw data','chinook','chinook_info.csv'));chinook_source<- read.csv(here('data','raw data','chinook','chinook_sources.csv')) 
cow_chin<- read.csv(here('data','raw data','chinook','Cowichan_chinook_broodtable.csv'),na.strings = c('#N/A'))
harrison_chin<-read.csv(here('data','raw data','chinook','Harrison_chinook_broodtable.csv'))
shuswap_chin<-read.csv(here('data','raw data','chinook','Lower_Shuswap_chinook_broodtable.csv'))
nicola_chin<-read.csv(here('data','raw data','chinook','Nicola_chinook_broodtable.csv'))
skeena_chin<-read.csv(here('data','raw data','chinook','ckSkeena.csv'))

#(partially) synonymize column names for sanity
colnames(sockeye)<- tolower(names(sockeye));colnames(sockeye_info)<- tolower(names(sockeye_info));colnames(sockeye_source)<- tolower(names(sockeye_source))
names(chum)<- gsub('.yr','year',names(chum)) #make the brood years equivalent
names(pink)<- gsub('.yr','year',names(pink))  #make the brood years equivalent
colnames(coho)<- tolower(names(coho));colnames(coho_info)<- tolower(names(coho_info));colnames(coho_source)<- tolower(names(coho_source));names(coho_source)[1]='source.id'
colnames(chinook)<- tolower(names(chinook));colnames(chinook_info)<- tolower(names(chinook_info));colnames(chinook_source)<- tolower(names(chinook_source))
names(cow_chin)[1]='stock.id'
names(psc_fraser_sockeye)[1]='stock';names(psc_fraser_sockeye)[3]='broodyear'
names(psc_fraser_sockeye)[9:21]=gsub('recruits_age','r',names(psc_fraser_sockeye)[9:21])
names(psc_fraser_sockeye)[9:21]=gr_to_euro(names(psc_fraser_sockeye)[9:21])
names(ifr_coho)[1]='stock';names(ifr_coho)[4]='spawners';names(ifr_coho)[5]='recruits'
names(harrison_chin)[1]='broodyear';names(harrison_chin)[4]='spawners';names(harrison_chin)[5]='recruits'
names(shuswap_chin)[1]='broodyear';names(shuswap_chin)[4]='spawners';names(shuswap_chin)[5]='recruits'
names(nicola_chin)[2]='broodyear';names(shuswap_chin)[4]='spawners';names(shuswap_chin)[5]='recruits'

#add ocean regions to info files
chum_info$ocean.region=sockeye_info$ocean.region[match(chum_info$region,sockeye_info$region)]
chum_info$ocean.region[1:9]='WC'; chum_info$ocean.region=replace_na(chum_info$ocean.region,"WC")
pink_info$ocean.region=chum_info$ocean.region[match(pink_info$region,chum_info$region)]
pink_info$ocean.region=replace_na(pink_info$ocean.region,"WC")

#Sockeye####
stock_dat=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
#General stock characteristics, start and end of time-series, number of useable years (removing years with useflags),average spawner and recruit 

#From the top - sockeye compilation

#Merge in new datasets for the old compilation
#updated data
#PSC for Fraser stocks
#skeena-nass stocks
#bristol bay & other Alaskan sockeye series from A. Munroe (ADFG)
fr_sock<- subset(sockeye, stock %in% psc_fraser_sockeye$stock)
bb_sock<<- subset(sockeye, stock %in% bb_soc[,1])

sockeye2<- subset(sockeye, stock %notin% psc_fraser_sockeye$stock) #Drop out older data for Fraser R stocks
sockeye2<- subset(sockeye2, stock %notin% bb_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% kasilof_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% kenai_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% mfgn_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% seak_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% ww_soc[,1]) #Drop out older data for Bristol Bay stocks
sockeye2<- subset(sockeye2, stock %notin% 'Skeena') #drop Skeena (have updated data in another source)
sockeye2<- subset(sockeye2, stock %notin% 'Nass (Meziadin)') #drop Meziadin (have updated data in another source)
sockeye2<- subset(sockeye2, stock %notin% 'Goodnews') #drop Goodnews (have updated data in another source)
sockeye2<- subset(sockeye2, stock %notin% 'Owikeno') #drop Goodnews (have updated data in another source)
#remove these stocks that are updated in the westward series (different names)
sockeye2<- subset(sockeye2, stock %notin% c('Early Upper Station','Late Upper Station','Chignik Lake','Early Karluk','Late Karluk'))
length(unique(sockeye2$stock))
length(unique(sockeye$stock))

#Process the remaining stock data
sockeye_list=list()
for(i in 1:length(unique(sockeye2$stock.id))){
  s=subset(sockeye,stock.id==unique(sockeye2$stock.id)[i])
  s_info<- subset(sockeye_info,stock.id==unique(sockeye2$stock.id)[i])
  s_use=subset(s,useflag==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  s_use<- subset(s_use,spawners!=0&recruits!=0)
  
  
  stock_dat[i,1]=NA
  stock_dat[i,2]=unique(s$species)
  stock_dat[i,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat[i,4]=unique(s_info$lat)
  stock_dat[i,5]=unique(s_info$lon)
  stock_dat[i,6]=unique(s_info$region)
  stock_dat[i,7]=unique(s_info$ocean.region)
  stock_dat[i,8]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat[i,9]=min(s_use$broodyear)
    stock_dat[i,10]=max(s_use$broodyear)
    stock_dat[i,11]=length(s_use$broodyear)
    stock_dat[i,12]=mean(s_use$spawners)/1e3
    stock_dat[i,13]=mean(s_use$recruits)/1e3
  }else{
    stock_dat[i,9]=NA
    stock_dat[i,10]=NA
    stock_dat[i,11]=0
    stock_dat[i,12:13]=NA
  }
  
  stock_dat[i,14]=sockeye_source$source[match(s_info$source.id,sockeye_source$source.id)]
  stock_dat[i,15]=s_info$comment..we.will.update.this.later.
  
  sockeye_list[[i]]=s_use[,c('stock','species','broodyear','spawners','recruits',names(s_use)[13:33])]
}

#Fraser sockeye stocks - PSC 2022 production dataset
psc_fraser_sockeye=psc_fraser_sockeye[grepl('Misc.',psc_fraser_sockeye$stock)==F,] #remove the miscellaneous stocks - mostly short series

for(i in 1:length(unique(psc_fraser_sockeye$stock))){
  s=subset(psc_fraser_sockeye,production_stock_code==unique(psc_fraser_sockeye$production_stock_code)[i])
  s= s %>% mutate(recruits = rowSums(s[,9:21],na.rm = TRUE))
  s$species<- rep('Sockeye',nrow(s))
  #to determine whether to use effective female spawners or total spawners based on data availability
  if(length(na.omit(s$total_broodyr_EFS))<length(na.omit(s$total_broodyr_spawners))){
   names(s)[4]='spawners' 
  }
  names(s)[5]='spawners' #use effective female spawners for these stocks
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=sockeye_info$lat[2] #lat for mouth of Fraser
  stock_dat_temp[,5]=sockeye_info$lon[2] #lon for mouth of Fraser
  stock_dat_temp[,6]='Fraser River' #Fraser River
  stock_dat_temp[,7]='WC' #West Coast
  stock_dat_temp[,8]='BC' #British Columbia
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Eric Taylor, Pacific Salmon Commission, 2022'

  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','spawners','recruits',names(s)[9:21])]
}

#skeena updated data
skeena_sockeye$stock=gsub('Bear','Bear-Skeena',skeena_sockeye$stock) #renaming Bear stock to avoid conflict with Bear R. Alaska
skeena_sockeye=subset(skeena_sockeye, stock %notin% c('Babine-Fulton','Babine-Pinkut')) #remove enhanced stocks from Skeena
for(i in 1:length(unique(skeena_sockeye$stock))){
  s=subset(skeena_sockeye,stock==unique(skeena_sockeye$stock)[i])
  s= s[complete.cases(s$spawners),];s=s[complete.cases(s$recruits),] #keep years with both spawner & recruit estimates
  if(nrow(s)==0){next}
  s$species<- rep('Sockeye',nrow(s))
 
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=54.22 #lat need to do these (one for Skeena estuary and one for Nass)
  stock_dat_temp[,5]=-129.831 #lon 
  stock_dat_temp[,6]='Skeena River'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Charmaine Carr-Harris, DFO, 2023'
  stock_dat_temp[,15]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','spawners','recruits',names(s)[7:11])]
}


#nass updated data
for(i in 1:length(unique(nass_sockeye$stock))){
  s=subset(nass_sockeye,stock==unique(nass_sockeye$stock)[i])
  s= s[complete.cases(s$spawners),];s=s[complete.cases(s$recruits),] #keep years with both spawner & recruit estimates
  if(nrow(s)==0){next}
  s$species<- rep('Sockeye',nrow(s))
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]= 54.9898 #lat - approx ocean entry from google maps
  stock_dat_temp[,5]=-130.02 #lon - approx ocean entry from google maps
  stock_dat_temp[,6]='Nass River'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Charmaine Carr-Harris, DFO, 2023'
  stock_dat_temp[,15]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','spawners','recruits',names(s)[7:11])]
}

#PSE - other Sockeye stocks
pse_soc=subset(pse_df,species_name=='Lake sockeye')
s_info_soc=distinct(pse_soc,cu_name_pse,.keep_all=T)
s_info_soc$region=gsub('Vancouver Island & Mainland Inlets','Vancouver Island',s_info_soc$region)

#lat/lons for each stock
s_info_soc$lat=NA;s_info_soc$lon=NA
s_info_soc$lat[1]=t=51.68;s_info_soc$lon[1]=-127.306 #owikeno
s_info_soc$lat[2]=t=53.08;s_info_soc$lon[2]=-128.555 #canoona
s_info_soc$lat[3]=t=53.56;s_info_soc$lon[3]=-128.958 #evelyn
s_info_soc$lat[4]=t=52.75;s_info_soc$lon[4]=-127.886 #kainet cr
s_info_soc$lat[5]=t=52.246;s_info_soc$lon[5]=-127.892 #kitelope
s_info_soc$lat[6]=t=52.87;s_info_soc$lon[6]=-128.69 #bloomfield
s_info_soc$lat[7]=t=53.45;s_info_soc$lon[7]=-129.787 #devon l
s_info_soc$lat[8]=t=53.42;s_info_soc$lon[8]=-129.247 #hartley b
s_info_soc$lat[9]=t=52.16;s_info_soc$lon[9]=-128.046 #Kadjusdis R
s_info_soc$lat[10]=t=53.31;s_info_soc$lon[10]=-129.825 #Keecha R
s_info_soc$lat[11]=t=53.34;s_info_soc$lon[11]=-129.867 #Kooryet R
s_info_soc$lat[12]=t=53.56;s_info_soc$lon[12]=-129.574 #Lower/Simpson/WEir
s_info_soc$lat[13]=t=53.43;s_info_soc$lon[13]=-129.836 #Mikado cr
s_info_soc$lat[14]=t=51.86;s_info_soc$lon[14]=-127.868 #Namu R.
s_info_soc$lat[15]=t=52.12;s_info_soc$lon[15]=-127.852 #Port John
s_info_soc$lat[16]=t=52.29;s_info_soc$lon[16]=-128.259 #Tankeeah Riv.
s_info_soc$lat[17]=t=53.36;s_info_soc$lon[17]=-129.46 #Tsimtack/Moore/Roger
s_info_soc$lat[18]=t=52.295;s_info_soc$lon[18]=-128.115 #Yeo L
s_info_soc$lat[19]=t=50.576;s_info_soc$lon[19]=-126.96 #Nimpkish L
s_info_soc$lat[20]=t=53.665;s_info_soc$lon[20]=-132.523 #Awun L
s_info_soc$lat[21]=t=53.975;s_info_soc$lon[21]=-132.643 #Marian/Eden L
s_info_soc$lat[22]=t=53.59;s_info_soc$lon[22]=-132.907 #Mercer L
s_info_soc$lat[23]=t=53.167;s_info_soc$lon[23]=-131.789 #Skidgate L
s_info_soc$lat[24]=t=53.682;s_info_soc$lon[24]=-132.235 #Yakoun L


for(i in 1:nrow(s_info_soc)){
  s=subset(pse_soc,stock==s_info_soc$stock[i])
  s= s[complete.cases(s$spawners),];s=s[complete.cases(s$recruits),] #keep years with both spawner & recruit estimates
  if(nrow(s)==0){next}
  s$species<- rep('Sockeye',nrow(s))
  s$stock=s$cu_name_pse
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$cu_name_pse),'Sockeye',sep='-')
  stock_dat_temp[,4]=s_info_soc$lat[i]
  stock_dat_temp[,5]=s_info_soc$lon[i] #lon - approx ocean entry from google maps
  stock_dat_temp[,6]=s_info_soc$region[i]
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$year)
  stock_dat_temp[,10]=max(s$year)
  stock_dat_temp[,11]=length(s$year)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Salmon Watersheds Program, PSF, 2022'
  stock_dat_temp[,15]=NA
  colnames(s)[5]='broodyear'
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','spawners','recruits')]
}

#Bristol bay
bb_info<- subset(sockeye_info,stock %in% bb_soc[,1])
#remove commas and convert to numeric
for(t in 3:22){
bb_soc[,t]=as.numeric(gsub(',','',bb_soc[,t]))
}


for(i in 1:length(unique(bb_soc[,1]))){
  s=subset(bb_soc,bb_soc[,1]==unique(bb_soc[,1])[i])
  s=s[complete.cases(s$R.S),]
  
  names(s)=c('stock','broodyear','r0.1','r0.2','r0.3','r0.4','r0.5','r1.1','r1.2','r1.3','r1.4','r1.5',
             'r2.1','r2.2','r2.3','r2.4','r3.1','r3.2','r3.3','r3.4','spawners','recruits','RS')
  s$species='Sockeye'
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=bb_info$lat[i] #lat 
  stock_dat_temp[,5]=bb_info$lon[i] #lon
  stock_dat_temp[,6]='Bristol Bay' #Bering Sea
  stock_dat_temp[,7]='BS' #Bering Sea
  stock_dat_temp[,8]='AK' #Alaska
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='A. Munro,2023'
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners',names(s)[3:20])]
}

#Kasilof Sockeye
names(kasilof_soc)[1:3]=c('stock','broodyear','spawners')
names(kasilof_soc)[4:17]=gsub('X','r',names(kasilof_soc)[4:17])
names(kasilof_soc)[18:21]=c('run','recruits','catch','RS')
kasilof_soc$species='Sockeye'

kasilof_soc=kasilof_soc[complete.cases(kasilof_soc$RS),]

stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)

kasilof_info=subset(sockeye_info,stock=='Kasilof')
stock_dat_temp[,2]='Sockeye'
stock_dat_temp[,3]=paste(unique(kasilof_soc$stock),'Sockeye',sep='-')
stock_dat_temp[,4]=kasilof_info$lat #lat 
stock_dat_temp[,5]=kasilof_info$lon #lon
stock_dat_temp[,6]=kasilof_info$region #Gulf of Alaska
stock_dat_temp[,7]='GOA' #Gulf of Alaska
stock_dat_temp[,8]='AK' #Alaska

stock_dat_temp[,9]=min(kasilof_soc$broodyear)
stock_dat_temp[,10]=max(kasilof_soc$broodyear)
stock_dat_temp[,11]=length(kasilof_soc$broodyear)
stock_dat_temp[,12]=mean(kasilof_soc$spawners)/1e3
stock_dat_temp[,13]=mean(kasilof_soc$recruits)/1e3
stock_dat_temp[,14]='A. Munro,2023'

stock_dat=rbind(stock_dat,stock_dat_temp)

sockeye_list[[nrow(stock_dat)]]=kasilof_soc[,c('stock','species','broodyear','recruits','spawners',names(kasilof_soc)[4:17])]

#Kenai
names(kenai_soc)[1:3]=c('stock','broodyear','spawners')
names(kenai_soc)[5:18]=gsub('X','r',names(kenai_soc)[5:18])
names(kenai_soc)[19:22]=c('run','recruits','catch','RS')
kenai_soc$species='Sockeye'

kenai_soc=kenai_soc[complete.cases(kenai_soc$RS),]

stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)

kenai_info=subset(sockeye_info,stock=='Kenai')
stock_dat_temp[,2]='Sockeye'
stock_dat_temp[,3]=paste(unique(kenai_soc$stock),'Sockeye',sep='-')
stock_dat_temp[,4]=kenai_info$lat #lat 
stock_dat_temp[,5]=kenai_info$lon #lon
stock_dat_temp[,6]=kenai_info$region #Gulf of Alaska
stock_dat_temp[,7]='GOA' #Gulf of Alaska
stock_dat_temp[,8]='AK' #Alaska

stock_dat_temp[,9]=min(kenai_soc$broodyear)
stock_dat_temp[,10]=max(kenai_soc$broodyear)
stock_dat_temp[,11]=length(kenai_soc$broodyear)
stock_dat_temp[,12]=mean(kenai_soc$spawners)/1e3
stock_dat_temp[,13]=mean(kenai_soc$recruits)/1e3
stock_dat_temp[,14]='A. Munro,2023'

stock_dat=rbind(stock_dat,stock_dat_temp)

sockeye_list[[nrow(stock_dat)]]=kenai_soc[,c('stock','species','broodyear','recruits','spawners',names(kenai_soc)[5:18])]

#Goodnews
head(mfgn_soc)
names(mfgn_soc)[1:3]=c('stock','broodyear','spawners')
names(mfgn_soc)[4:16]=gsub('X','r',names(mfgn_soc)[4:16])
names(mfgn_soc)[17:18]=c('recruits','RS')
mfgn_soc$species='Sockeye'

mfgn_soc=mfgn_soc[complete.cases(mfgn_soc$RS),]
mfgn_soc=subset(mfgn_soc,recruits!=0)#remove estimates without full recruits

stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)

mfgn_info=subset(sockeye_info,stock=='Goodnews')
stock_dat_temp[,2]='Sockeye'
stock_dat_temp[,3]=paste(unique(mfgn_soc$stock),'Sockeye',sep='-')
stock_dat_temp[,4]=mfgn_info$lat #lat 
stock_dat_temp[,5]=mfgn_info$lon #lon
stock_dat_temp[,6]=mfgn_info$region
stock_dat_temp[,7]='BS' #Bering sea
stock_dat_temp[,8]='AK' #Alaska

stock_dat_temp[,9]=min(mfgn_soc$broodyear)
stock_dat_temp[,10]=max(mfgn_soc$broodyear)
stock_dat_temp[,11]=length(mfgn_soc$broodyear)
stock_dat_temp[,12]=mean(mfgn_soc$spawners)/1e3
stock_dat_temp[,13]=mean(mfgn_soc$recruits)/1e3
stock_dat_temp[,14]='A. Munro,2023'

stock_dat=rbind(stock_dat,stock_dat_temp)

sockeye_list[[nrow(stock_dat)]]=mfgn_soc[,c('stock','species','broodyear','recruits','spawners',names(mfgn_soc)[4:16])]

#SEAK stocks
names(seak_soc)[1:4]=c('stock','broodyear','spawners','recruits')
names(seak_soc)[5:11]=c('r2','r3','r4','r5','r6','r7','r8')
seak_soc$species='Sockeye'

for(i in 1:length(unique(seak_soc$stock))){
  s=subset(seak_soc,seak_soc$stock==unique(seak_soc$stock)[i])
  s=s[complete.cases(s$spawners&s$recruits),]
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=sockeye_info$lat[match(unique(s$stock),sockeye_info$stock)] #lat 
  stock_dat_temp[,5]=sockeye_info$lon[match(unique(s$stock),sockeye_info$stock)] #lon
  stock_dat_temp[,6]=sockeye_info$region[match(unique(s$stock),sockeye_info$stock)] 
  stock_dat_temp[,7]='GOA' #Bering Sea
  stock_dat_temp[,8]='AK' #Alaska
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='A. Munro,2023'

  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners',names(s)[5:11])]
}

#Westward stocks - kodiak
ww_info=data.frame(stock=unique(ww_soc$Stock))
ww_info$lat=sockeye_info$lat[match(ww_info$stock,sockeye_info$stock)]
ww_info$lon=sockeye_info$lon[match(ww_info$stock,sockeye_info$stock)]
ww_info$region=c(rep('Kodiak',5),rep('Chignik',2),rep('AYK',2))
ww_info$ocean.region=sockeye_info$ocean.region[match(ww_info$stock,sockeye_info$stock)]
ww_info[1:2,3]=sockeye_info$lon[sockeye_info$stock=='Late Upper Station']
ww_info[1:2,2]=sockeye_info$lat[sockeye_info$stock=='Late Upper Station']
ww_info[1:2,3]=sockeye_info$lon[sockeye_info$stock=='Late Upper Station']
ww_info[1:2,5]=sockeye_info$ocean.region[sockeye_info$stock=='Late Upper Station']
ww_info[4:5,3]=sockeye_info$lon[sockeye_info$stock=='Late Karluk']
ww_info[4:5,2]=sockeye_info$lat[sockeye_info$stock=='Late Karluk']
ww_info[4:5,3]=sockeye_info$lon[sockeye_info$stock=='Late Karluk']
ww_info[4:5,5]=sockeye_info$ocean.region[sockeye_info$stock=='Late Karluk']
ww_info[6:7,3]=sockeye_info$lon[sockeye_info$stock=='Chignik Lake']
ww_info[6:7,2]=sockeye_info$lat[sockeye_info$stock=='Chignik Lake']
ww_info[6:7,3]=sockeye_info$lon[sockeye_info$stock=='Chignik Lake']
ww_info[6:7,5]=sockeye_info$ocean.region[sockeye_info$stock=='Chignik Lake']

names(ww_soc)[1:3]=c('stock','broodyear','spawners')
names(ww_soc)[4:25]=gsub('X','r',names(ww_soc[4:25]))
ww_soc$recruits=rowSums(ww_soc[,4:25],na.rm=T)
ww_soc$species='Sockeye'

for(i in 1:length(unique(ww_soc[,1]))){
  s=subset(ww_soc,ww_soc[,1]==unique(ww_soc[,1])[i])
  s=s[complete.cases(s$r0.2),] #some early cohorts missing age classes
  s=subset(s,broodyear<=2015) #missing age classes past this year

  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Sockeye'
  stock_dat_temp[,3]=paste(unique(s$stock),'Sockeye',sep='-')
  stock_dat_temp[,4]=ww_info$lat[i] #lat 
  stock_dat_temp[,5]=ww_info$lon[i] #lon
  stock_dat_temp[,6]=ww_info$region[i] 
  stock_dat_temp[,7]=ww_info$ocean.region[i] 
  stock_dat_temp[,8]='AK' #Alaska

  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(as.numeric(gsub(',','',s$spawners)))/1e3
  stock_dat_temp[,13]=mean(as.numeric(gsub(',','',s$recruits)))/1e3
  stock_dat_temp[,14]='A. Munro,2023'
  stock_dat_temp[,15]=NA
  stock_dat_temp[,16:17]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  sockeye_list[[nrow(stock_dat)]]=s[,c('stock','species','broodyear','recruits','spawners',names(s)[4:25])]
}

#GCL & Sproat Sockeye
somass_soc=somass_soc[complete.cases(somass_soc$recruit_abundance),]
somass_soc$comb.age=paste('r',somass_soc$f_age,'.',somass_soc$o_age,sep='')

#GCL
gcl=subset(somass_soc,cu_name=='Great Central Lake')
gcl2=as.data.frame(pivot_wider(gcl,id_cols=c('cu_name','brood_year'),names_from = 'comb.age',values_from='recruit_abundance'))
gcl2$spawners=gcl$total_escape[match(gcl2$brood_year,gcl$adult_return_year)]
gcl2$recruits=apply(gcl2[,3:8],1,sum)

gcl.bt=gcl2[complete.cases(gcl2$recruits/gcl2$spawners),]
names(gcl.bt)[1:2]=c('stock','broodyear')
gcl.bt$species='Sockeye'

stock_dat_temp=data.frame(stock.id=NA,species='Sockeye',stock.name='Great Central Lake-Sockeye',lat=49.24,lon=-124.8,region='Vancouver Island',ocean.basin='WC',state='BC',begin=min(gcl.bt[,2]),end=max(gcl.bt[,2]),n.years=nrow(gcl.bt),m.spawners=mean(gcl.bt$spawners)/1e3,m.recruits=max(gcl.bt$recruits)/1e3,source='Colin Bailey, DFO, 2023',url=NA,comments=NA, age=NA)
stock_dat=rbind(stock_dat,stock_dat_temp)
sockeye_list[[nrow(stock_dat)]]=gcl.bt[,c('stock','species','broodyear','spawners','recruits',names(gcl.bt)[3:8])]

#Sproat
spt=subset(somass_soc,cu_name=='Sproat Lake')
#get escapement for each brood year
spt2=as.data.frame(pivot_wider(spt,id_cols=c('cu_name','brood_year'),names_from = 'comb.age',values_from='recruit_abundance'))
spt2$spawners=gcl$total_escape[match(spt2$brood_year,gcl$adult_return_year)]
spt2$recruits=apply(spt2[,3:8],1,sum)

spt.bt=spt2[complete.cases(spt2$recruits/spt2$spawners),]
names(spt.bt)[1:2]=c('stock','broodyear')
spt.bt$species='Sockeye'

stock_dat_temp=data.frame(stock.id=NA,species='Sockeye',stock.name='Sproat Lake-Sockeye',lat=49.24,lon=-124.8,region='Vancouver Island',ocean.basin='WC',state='BC',begin=min(spt.bt[,2]),end=max(spt.bt[,2]),n.years=nrow(spt.bt),m.spawners=mean(spt.bt$spawners)/1e3,m.recruits=mean(spt.bt$recruits)/1e3,source='Colin Bailey, DFO, 2023',url=NA,comments=NA, age=NA)
stock_dat=rbind(stock_dat,stock_dat_temp)
sockeye_list[[nrow(stock_dat)]]=spt.bt[,c('stock','species','broodyear','recruits','spawners',names(spt.bt)[3:8])]

#combine list of filtered datasets
sockeye_filtered = do.call(plyr::rbind.fill, sockeye_list)
length(unique(sockeye_filtered$stock))

#add age data flag to info sheet
#stock_dat$age <- NA
#for(i in 1:nrow(stock_dat)){
#  sub <- subset(sockeye_filtered, stock==stock_dat$stock.name[i])
#  r.cols <- grep("^r[[:digit:]]\\.[[:digit:]]", names(sub), value = TRUE)
#  stock_dat$age[i] <- ifelse(all(is.na(sub[,r.cols])), 0, 1)
#}


#Chum####
#Merge in updated WA pink series
chu_upd$stock=gsub("\\s*\\([^\\)]+\\)",'',chu_upd$stock) #to make names synonymous bwn datasets remove parentheses on stock names
chu_old<- subset(chum, stock %in% chu_upd$stock)
length(unique(chu_old$stock));length(unique(chu_upd$stock)) #1 new stock, 9 updated
names(chu_upd)[1]='stock.id'
chu_upd$stock.id=ifelse(chu_upd$stock.id==310,max(chum$stock.id)+1,chu_upd$stock.id) #remove duplicate stock ids, make new one for extra stock

#add info
chum_info[nrow(chum_info)+1,1:9]=c(max(chu_upd$stock.id),'Chum',unique(chu_upd$stock)[10],'Inside WA','Inside WA','WA',48.1618, 123.59,23)

chum_source[24,1:2]=c(24,'Marisa Litz, WDFW, 2023')
chum_info$source.id[1:9]=24
chum_info$ocean.region[nrow(chum_info)]='WC'

chum2<- subset(chum, stock %notin% chu_upd$stock) #Drop out older data for Fraser R stocks
chu_upd2=chu_upd[,1:9];
chu_upd2$recruits.2=NA;chu_upd2[,11:13]=chu_upd[,10:12];chu_upd2$recruits.6=NA;chu_upd2$recruits.7=NA
chu_upd2$age=chu_upd$age
names(chu_upd2)[6]='broodyear'

chum2<- rbind(chu_upd2,chum2)
length(unique(chum2$stock))

#
pse_chum=subset(pse_df,species_name=='Chum')
pse_chum$stock=pse_chum$cu_name_pse
pse_chum$species='Chum'

#additional PSE dataset with recruitment ages for some stocks - meld these in for HH project
pse_chum2 <- pse_ncc %>%
  subset(SpeciesId == "CM") %>%
  filter(CU_Name %in% pse_chum$cu_name_pse) %>% #only keep series with 'fair' + data quality
  drop_na(TR6) %>% # drop incomplete brood years (i.e., without recruits for 5 year olds)
  rename(stock = CU_Name) %>%
  left_join(chum_info, by = "stock")  %>%
  mutate(species = "Chum",
         use = 1,
         age = "avg") %>%
  rename(broodyear = BroodYear,
         spawners = Escape,
         recruits = Total,
         recruits.2 = TR2,
         recruits.3 = TR3,
         recruits.4 = TR4,
         recruits.5 = TR5,
         recruits.6 = TR6, 
         recruits.7 = TR7) %>%
  select(species, stock, region, sub.region, broodyear, spawners, recruits, use,recruits.2, recruits.3, recruits.4, recruits.5, recruits.6, recruits.7, age)

##

pse_chum[,14:19]=pse_chum2[match(paste(pse_chum$cu_name_pse,pse_chum$year),paste(pse_chum2$stock,pse_chum2$broodyear)),9:14]


chum3<- dplyr::full_join(chum2,pse_chum)

#remove old statistical area run-reconstructions
chum_info$lon=c(0-as.numeric(chum_info$lon))
chum_info<- subset(chum_info, stock %notin% c("Area 10", "Area 9", "Area 8", "Area 7", "Area 6", "Area 5", "Area 4", "Area 3", "Area 2W", "Area 2E", "Area 1"))
chum3<- subset(chum3, stock %notin% c("Area 10", "Area 9", "Area 8", "Area 7", "Area 6", "Area 5", "Area 4", "Area 3", "Area 2W", "Area 2E", "Area 1"))


chum_list=list()
for(i in 1:length(unique(chum3$stock))){
  s=subset(chum3,stock==unique(chum3$stock)[i])
  s_info<- subset(chum_info,stock==unique(chum3$stock)[i])
  s_use=subset(s,is.na(spawners)==F&is.na(recruits)==F)
  s_age<- unique(s$age[s$age != ""])
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$sub.region)
  stock_dat_temp[,7]=unique(s_info$ocean.region)
  stock_dat_temp[,8]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,9]=min(s_use$broodyear)
    stock_dat_temp[,10]=max(s_use$broodyear)
    stock_dat_temp[,11]=length(s_use$broodyear)
    stock_dat_temp[,12]=mean(s_use$spawners)/1e3
    stock_dat_temp[,13]=mean(s_use$recruits)/1e3
    }else{
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=NA
    stock_dat_temp[,11]=0
    stock_dat_temp[,12:13]=NA
  }
 
  s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
  if(length(s.id)==1){ stock_dat_temp[,14]=chum_source$source[match(s_info$source.id[1],chum_source$source.id)]
}
  if(length(s.id)==2){
    source<- subset(chum_source, source.id %in% s.id)
    stock_dat_temp[,14]=paste(source$source[1],source$source[2],sep='; ')
  }
  stock_dat_temp[,16]=s_info$comment[1]
  stock_dat_temp[,17]=ifelse(length(s_age)==1 && s_age=="data", 1, 0)
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chum_list[[i]]=s_use[,c('stock','species','broodyear','spawners','recruits','recruits.2','recruits.3','recruits.4','recruits.5','recruits.6','recruits.7')]
}
chum_filtered<- do.call(plyr::rbind.fill, chum_list)
#turn recruits into r to match others
names(chum_filtered)[6:11]=gsub('recruits.','r',names(chum_filtered)[6:11])

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
pink_info[nrow(pink_info)+1,1:9]=c(max(pi_upd$stock.id),'Pink',unique(pi_upd$stock)[9],'Inside WA','Inside WA','WA',47.16, -122.91,7)
pink_info$ocean.region[c(nrow(pink_info)-1,nrow(pink_info))]='WC'
pink_source[7,1:2]=c(7,'Marisa Litz, WDFW, April 2023')
pink_info$source.id[1:7]=7

pink2<- subset(pink, stock %notin% pi_upd$stock) #Drop out older data for Fraser R stocks
pi_upd2=pi_upd[,1:9]
names(pi_upd2)[6]='broodyear'

pink2<- rbind(pi_upd2,pink2)
length(unique(pink2$stock))

#add in PSE pink data and generate a file for H. Hunter project
pse_pk=subset(pse_df,species_name=='Pink (even)'|species_name=='Pink (odd)')
pse_pk$stock=pse_pk$cu_name_pse
pse_pk$region=gsub('Fraser','Fraser R',pse_pk$region)
pse_pk$region=gsub('Vancouver Island & Mainland Inlets','Vancouver Island',pse_pk$region)
pse_pk=subset(pse_pk,stock!='Fraser River (odd)') #have updated PSC data for this stock
pse_pk$use=1
pse_pk$broodyear=pse_pk$year


pink3<- dplyr::full_join(pink2,pse_pk)

pink3$stock<- gsub(" (even)", "", pink3$stock, fixed=T) # drop (even) and (odd)
pink3$stock<- gsub(" (odd)", "", pink3$stock, fixed=T)


pink3 <- pink3 %>% #drop north and central coast stocks that have been replaced with CU level reconstructions from PSE
  filter(!stock %in% c("Area 10", "Area 9", "Area 8", "Area 7", "Area 6", "Area 5", "Area 4", "Area 3", "Area 2W", "Area 2E", "Area 1","BC South (no Fraser)") )

pink_info$lon=0-as.numeric(pink_info$lon)

pink_list=list()
for(i in 1:length(unique(pink3$stock))){
  s=subset(pink3,stock==unique(pink3$stock)[i])
  s_info<- subset(pink_info,stock==unique(pink3$stock)[i])
  s_use=subset(s,use==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F) %>% subset(spawners>1)
  s_use<- subset(s_use,spawners!=0&recruits!=0)
  odd.yrs=ifelse(s_use$broodyear %% 2 == 0,0,1)
  s_use$odd=odd.yrs
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  if(length(levels(factor(odd.yrs)))>1){
    s_even=subset(s_use,odd==0)
    s_odd=subset(s_use,odd==1)
    s_even$species='Pink-Even'
    s_odd$species='Pink-Odd'
    
    stock_dat_temp[1,2]='Pink-Even'
    stock_dat_temp[2,2]='Pink-Odd'
    stock_dat_temp[1,3]=paste(unique(s$stock),'Pink-Even',sep='-')
    stock_dat_temp[2,3]=paste(unique(s$stock),'Pink-Odd',sep='-')
    stock_dat_temp[,4]=unique(s_info$lat)
    stock_dat_temp[,5]=unique(s_info$lon)
    stock_dat_temp[,6]=unique(s_info$sub.region)
    stock_dat_temp[,7]=unique(s_info$ocean.region)
    stock_dat_temp[,8]=unique(s_info$jurisdiction)
    
    if(nrow(s_use)!=0){
      stock_dat_temp[1,9]=min(s_even$broodyear)
      stock_dat_temp[1,10]=max(s_even$broodyear)
      stock_dat_temp[1,11]=length(s_even$broodyear)
      stock_dat_temp[1,12]=mean(s_even$spawners)/1e3
      stock_dat_temp[1,13]=mean(s_even$recruits)/1e3
      stock_dat_temp[2,9]=min(s_odd$broodyear)
      stock_dat_temp[2,10]=max(s_odd$broodyear)
      stock_dat_temp[2,11]=length(s_odd$broodyear)
      stock_dat_temp[2,12]=mean(s_odd$spawners)/1e3
      stock_dat_temp[2,13]=mean(s_odd$recruits)/1e3
    }else{
      stock_dat_temp[,9]=NA
      stock_dat_temp[,10]=NA
      stock_dat_temp[,11]=0
      stock_dat_temp[,12:13]=NA
    }
    
    s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
    if(length(s.id)==1){stock_dat_temp[,14]=pink_source$source[match(s_info$source.id,pink_source$source.id)]
    }
    if(length(s.id)==2){
      source<- subset(pink_source, source.id %in% s.id)
      stock_dat_temp[,14]=paste(source$source[1],source$source[2],sep='; ')
    }
    stock_dat_temp[,16]=s_info$comment
    stock_dat_temp[,17]=NA
    
    
    stock_dat=rbind(stock_dat,stock_dat_temp)
    
    pink_list[[i]]=rbind(s_even[,c('stock','species','broodyear','recruits','spawners')],s_odd[,c('stock','species','broodyear','recruits','spawners')])
  }else{
    s_use$species=ifelse(all(odd.yrs==0)==T,'Pink-Even','Pink-Odd')
    
    stock_dat_temp[,2]=unique(s_use$species)
    stock_dat_temp[,3]=paste(unique(s_use$stock),unique(s_use$species),sep='-')
    stock_dat_temp[,4]=unique(s_info$lat)
    stock_dat_temp[,5]=unique(s_info$lon)
    stock_dat_temp[,6]=unique(s_info$sub.region)
    stock_dat_temp[,7]=unique(s_info$ocean.region)
    stock_dat_temp[,8]=unique(s_info$jurisdiction)
    
    if(nrow(s_use)!=0){
      stock_dat_temp[,9]=min(s_use$broodyear)
      stock_dat_temp[,10]=max(s_use$broodyear)
      stock_dat_temp[,11]=length(s_use$broodyear)
      stock_dat_temp[,12]=mean(s_use$spawners)/1e3
      stock_dat_temp[,13]=mean(s_use$recruits)/1e3
    }else{
      stock_dat_temp[,9]=NA
      stock_dat_temp[,10]=NA
      stock_dat_temp[,11]=0
      stock_dat_temp[,12:13]=NA
    }
    
    s.id=as.numeric(strsplit(s_info$source.id,',')[[1]])
    if(length(s.id)==1){stock_dat_temp[,14]=pink_source$source[match(s_info$source.id,pink_source$source.id)]
    }
    if(length(s.id)==2){
      source<- subset(pink_source, source.id %in% s.id)
      stock_dat_temp[,14]=paste(source$source[1],source$source[2],sep='; ')
    }
    stock_dat_temp[,16]=s_info$comment
    stock_dat_temp[,17]=NA
    
    
    stock_dat=rbind(stock_dat,stock_dat_temp)
    
    pink_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners')]
  }
}
pink_filtered<- do.call(plyr::rbind.fill, pink_list)


#write.csv(pink_filtered,here('data','filtered datasets',paste('raw_pink_brood_table',Sys.Date(),'.csv',sep='')),row.names = FALSE)
#write.csv(pink_info,here('data','filtered datasets',paste('pink_info',Sys.Date(),'.csv',sep='')), row.names = FALSE)

#Chinook####
#Add in Cowichan chinook and update info
chinook=rbind(chinook,cow_chin) #add in S-R data
chinook_info$sub.region=gsub('Southeast','SEAK',chinook_info$sub.region) #synonymize the sub regions
chinook_info$sub.region=gsub('Kuskokwim','AYK',chinook_info$sub.region) #synonymize the sub regions
chinook_info$sub.region=gsub('Norton Sound','AYK',chinook_info$sub.region) #synonymize the sub regions
chinook_info$sub.region=gsub('Alaska Peninsula and Aleutian Islands','AK Peninsula',chinook_info$sub.region) #synonymize the sub regions


chinook_info[21,1:7]=c(333,'Chinook','Cowichan','WC','BC South','BC South','BC');chinook_info$lat[21]=48.7581;chinook_info$lon[21]=-123.6242;chinook_info$source.id[21]=11 #add in metadata
chinook_info[22,1:7]=c(334,'Chinook','Harrison','WC','Fraser River','Fraser River','BC');chinook_info$lat[22]=sockeye_info$lat[2];chinook_info$lon[22]=sockeye_info$lon[2];chinook_info$source.id[22]=12 #add in metadata
chinook_info[23,1:7]=c(335,'Chinook','Lower Shuswap','WC','Fraser River','Fraser River','BC');chinook_info$lat[23]=sockeye_info$lat[2];chinook_info$lon[23]=sockeye_info$lon[2];chinook_info$source.id[23]=12 #add in metadata
chinook_info[24,1:7]=c(336,'Chinook','Nicola','WC','Fraser River','Fraser River','BC');chinook_info$lat[24]=sockeye_info$lat[2];chinook_info$lon[24]=sockeye_info$lon[2];chinook_info$source.id[24]=13 #add in metadata
chinook_source[10,1]=11;chinook_source[11,3]='Karalea Cantera, DFO, 2022' #add in source
chinook_source[11,1]=12;chinook_source[12,3]='Chucken Parken, DFO, 2022' #add in source
chinook_source[12,1]=13;chinook_source[13,3]='Luke Warkentin, DFO, 2022' #add in source

chinook_list=list()
for(i in 1:length(unique(chinook$stock.id))){
  s=subset(chinook,stock.id==unique(chinook$stock.id)[i])
  s_info<- subset(chinook_info,stock.id==unique(chinook$stock.id)[i])
  s_use=subset(s,useflag==1) %>% subset(is.na(spawners)==F&is.na(recruits)==F)
  s_use<- subset(s_use,spawners!=0&recruits!=0)
  
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  

  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$sub.region)
  stock_dat_temp[,7]=unique(s_info$ocean.region)
  stock_dat_temp[,8]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,9]=min(s_use$broodyear)
    stock_dat_temp[,10]=max(s_use$broodyear)
    stock_dat_temp[,11]=length(s_use$broodyear)
    stock_dat_temp[,12]=mean(s_use$spawners)/1e3
    stock_dat_temp[,13]=mean(s_use$recruits)/1e3
  }else{
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=NA
    stock_dat_temp[,11]=0
    stock_dat_temp[,12:13]=NA
  }
  
  
  s.id=as.numeric(strsplit(as.character(s_info$source.id),',')[[1]])
  if(length(s.id)==1){
    stock_dat_temp[,14]=chinook_source$title[match(s_info$source.id,sockeye_source$source.id)]
    stock_dat_temp[,15]=chinook_source$url[match(s_info$source.id,sockeye_source$source.id)]
    
  }
  if(length(s.id)==2){
    source<- subset(chinook_source, source.id %in% s.id)
    stock_dat_temp[,14]=paste(source$title[1],source$title[2],sep='; ')
    stock_dat_temp[,15]=paste(source$url[1],source$url[2],sep='; ')
  }
  stock_dat_temp[,16]=NA #no comments
  stock_dat_temp[,17]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chinook_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners','r2','r3','r4','r5','r6','r7','r8')]
}

#Harrison chinook
harrison_chin3=subset(harrison_chin,is.na(recruits)==F&is.na(spawners)==F) #remove years without recruit data

row.n=nrow(stock_dat)+1
stock_dat[row.n,1]=NA #no pre-assigned stock id
stock_dat[row.n,2]=chinook_info[22,2]
stock_dat[row.n,3]=paste(chinook_info[22,3],chinook_info[22,2],sep='-')
stock_dat[row.n,4]=chinook_info[22,8]
stock_dat[row.n,5]=chinook_info[22,9]
stock_dat[row.n,6]='Fraser River'
stock_dat[row.n,7]='WC'
stock_dat[row.n,8]='BC'
stock_dat[row.n,9]=min(harrison_chin3$broodyear)
stock_dat[row.n,10]=max(harrison_chin3$broodyear)
stock_dat[row.n,11]=length(harrison_chin3$broodyear)
stock_dat[row.n,12]=mean(harrison_chin3$spawners)/1e3
stock_dat[row.n,13]=mean(harrison_chin3$recruits)/1e3
stock_dat[row.n,14]=chinook_source$title[chinook_info$source.id[22]]

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
stock_dat[row.n,6]='Fraser River'
stock_dat[row.n,7]='WC'
stock_dat[row.n,8]='BC'
stock_dat[row.n,9]=min(shuswap_chin$broodyear)
stock_dat[row.n,10]=max(shuswap_chin$broodyear)
stock_dat[row.n,11]=length(shuswap_chin$broodyear)
stock_dat[row.n,12]=mean(shuswap_chin$spawners)/1e3
stock_dat[row.n,13]=mean(shuswap_chin$recruits)/1e3
stock_dat[row.n,14]=chinook_source$title[chinook_info$source.id[23]]

shuswap_chin$stock=rep('Lower Shuswap',nrow(shuswap_chin))
shuswap_chin$species=rep('Chinook',nrow(shuswap_chin))
shuswap_chin=subset(shuswap_chin,is.na(recruits)==F&is.na(spawners)==F) #remove years without recruit data


chinook_list[[23]]=shuswap_chin[,c('stock','species','broodyear','recruits','spawners')]

#Nicola chinook
names(nicola_chin)[3]='recruits'
names(nicola_chin)[4]='spawners'
#use total spawners as per Warkentin et al. 2020
nicola_chin$stock=rep('Nicola',nrow(nicola_chin))
nicola_chin$species=rep('Chinook',nrow(nicola_chin))

row.n=nrow(stock_dat)+1
stock_dat[row.n,2]=chinook_info[24,2]
stock_dat[row.n,3]=paste(chinook_info[24,3],chinook_info[24,2],sep='-')
stock_dat[row.n,4]=chinook_info[24,8]
stock_dat[row.n,5]=chinook_info[24,9]
stock_dat[row.n,6]='Fraser River'
stock_dat[row.n,7]='WC'
stock_dat[row.n,8]='BC'
stock_dat[row.n,9]=min(nicola_chin$broodyear)
stock_dat[row.n,10]=max(nicola_chin$broodyear)
stock_dat[row.n,11]=length(nicola_chin$broodyear)
stock_dat[row.n,12]=mean(nicola_chin$spawners)/1e3
stock_dat[row.n,13]=mean(nicola_chin$recruits)/1e3
stock_dat[row.n,14]=chinook_source$title[chinook_info$source.id[24]]

chinook_list[[24]]=nicola_chin[,c('stock','species','broodyear','recruits','spawners')]


#Skeena Chinook - Withler et al. 
skeena_chin=subset(skeena_chin,Stocks!=unique(skeena_chin$Stocks)[3]) #remove tyee test fishery aggregate, since other series are the CU-specific stock recruit estimates
skeena_chin=skeena_chin[complete.cases(skeena_chin[,3:5]),]
names(skeena_chin)=c('broodyear','spawners','r4','r5','r6','recruits','stock')
skeena_chin$stock=gsub('_',' ',skeena_chin$stock)
cl=length(chinook_list)
for(i in 1:length(unique(skeena_chin$stock))){
  s=subset(skeena_chin,stock==unique(skeena_chin$stock)[i])
  s$species='Chinook'
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Chinook'
  stock_dat_temp[,3]=paste(unique(s$stock),'Chinook',sep="-")
  stock_dat_temp[,4]=54.2237 #lat need to do these (one for Skeena estuary and one for Nass)
  stock_dat_temp[,5]=-129.831 #lon 
  stock_dat_temp[,6]='Skeena River'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Ivan Winther, DFO, 2024'
  stock_dat_temp[,15]='https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41241046.pdf'
  stock_dat_temp[,17]='data'

  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chinook_list[[cl+i]]=s[,c('stock','species','broodyear','recruits','spawners','r4','r5','r6')]
}

#PSE chinook series
pse_chin=subset(pse_df,species_name=='Chinook')
pse_chin$stock=pse_chin$cu_name_pse

chin_pse_lat=c(51.68,52.31,52.78)
chin_pse_lon=c(-127.304,-126.99,-126.977)
cl=length(chinook_list)
for(i in 1:length(unique(pse_chin$stock))){
  s=subset(pse_chin,stock==unique(pse_chin$stock)[i])
  s$species='Chinook'
  s$broodyear=s$year
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Chinook'
  stock_dat_temp[,3]=paste(unique(s$stock),'Chinook',sep="-")
  stock_dat_temp[,4]=chin_pse_lat[i] #lat need to do these (one for Skeena estuary and one for Nass)
  stock_dat_temp[,5]=chin_pse_lon[i] #lon 
  stock_dat_temp[,6]='Central Coast'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$year)
  stock_dat_temp[,10]=max(s$year)
  stock_dat_temp[,11]=length(s$year)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Salmon Watersheds Program, PSF, 2022'
  stock_dat_temp[,15]=NA
  stock_dat_temp[,17]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chinook_list[[cl+i]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

#NOAA SPS dataset ###
sps=read.csv(here::here('data','raw data','multispecies','SPS_2022 Biological Viability Assessment.csv'),na.strings = c('-99','-99.00'))
sps_sub=subset(sps,SPECIES=='Chinook salmon') #subset to chinook, all other species have more updated data above
sps_sub=subset(sps_sub,POPULATION_NAME!='Mid-Hood Canal Chinook') #drop this stock, as some a number of age proportions are off (e.g. negative proportions)
sps_sub=subset(sps_sub,POPULATION_NAME!='Skykomish Chinook (Snohomish)') #drop this stock, not enough data
sps_sub=subset(sps_sub,CATCH>=0)
sps_sub$stock=gsub(' Chinook','',sps_sub$POPULATION_NAME)
#Approximate lat/lons for these stocks taken from google maps - at mouth of the river system
lat_lons<- data.frame(stock=unique(paste(sps_sub$POPULATION_NAME)),region='Inside WA',river.system=NA,lat=NA,lon=NA)

lat_lons[1,3:5]=cbind('Cedar River',47.5039,-122.2169) #Cedar River lat/lon
lat_lons[2,3:5]=cbind('Dungeness River',48.159,-123.135) #dungeness river - olympic peninsula
lat_lons[3,3:5]=cbind('Elwha River',48.170,-123.590) #Elwha river - olympic peninsula
lat_lons[4,3:5]=cbind('Green River',47.5925,-122.3600)
lat_lons[5,3:5]=cbind('Sauk (Skagit) River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[6,3:5]=cbind('Skagit River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[7,3:5]=cbind('Nisqually River',47.0996,-122.7029) #Nisqually river
lat_lons[8,3:5]=cbind('Nooksack River',48.766,-123.58) #Nooksack river - Bellingham bay
lat_lons[9,3:5]=cbind('Stillaguamish River',48.2465,-122.3957) #Stillaguimish river
lat_lons[10,3:5]=cbind('Puyallup River',47.2472,-122.4289) #Puyallup river & white river tributary
lat_lons[11,3:5]=cbind('Sammamish River',47.751,-122.266) #Nooksack river - Bellingham bay
lat_lons[12,3:5]=cbind('Skokomish River',47.3436,-123.1212) #Skokomish river & white river tributary
lat_lons[13,3:5]=cbind('Snohomisin River',48.0206,-122.2122) #Tributaries of Snohomisin (Snoqualmie & Skykomish)
lat_lons[14,3:5]=cbind('Nooksack River',48.766,-123.58) #Nooksack river - Bellingham bay
lat_lons[15,3:5]=cbind('Stillaguamish River',48.2465,-122.3957) #Stillaguimish river
lat_lons[16,3:5]=cbind('Suiattle (Skagit) River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[17,3:5]=cbind('Cascade (Skagit) River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[18,3:5]=cbind('Sauk (Skagit) River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[19,3:5]=cbind('Skagit River',48.3677,-122.5141) #Skagit river & sauk/suiattle tributaries lat/lon
lat_lons[20,3:5]=cbind('White River',47.305,-122.471) #Skagit river & sauk/suiattle tributaries lat/lon

cl=length(chinook_list)

sps_bt=list()
for(i in 1:length(unique(sps_sub$POPULATION_NAME))){
  
  #run reconstruction from escapement + harvest + p. age returns
  x=subset(sps_sub,POPULATION_NAME==levels(factor(sps_sub$POPULATION_NAME))[i])
  x=x[order(x$YEAR),]
  x$RUN_SIZE=x$NUMBER_OF_SPAWNERS*x$FRACWILD+x$CATCH
  x=x[is.na(x$FRACWILD)==F,]
  
  if(nrow(x)==0){next}
  
  bt_temp=data.frame(stock=unique(x$stock),broodyear=x$YEAR,species='Chinook',run=round(x$RUN_SIZE),spawners=round(x$NUMBER_OF_SPAWNERS),recruits=NA,r1=NA,r2=NA,r3=NA,r4=NA,r5=NA,r6=NA,r7=NA,frac.wild=x$FRACWILD)
  for(t in 1:nrow(x)){
    if(is.na(match(x$YEAR[t]-1,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-1,bt_temp$broodyear),6]=round(x$RUN_SIZE[t]*x$AGE_1_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-2,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-2,bt_temp$broodyear),7]=round(x$RUN_SIZE[t]*x$AGE_2_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-3,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-3,bt_temp$broodyear),8]=round(x$RUN_SIZE[t]*x$AGE_3_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-4,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-4,bt_temp$broodyear),9]=round(x$RUN_SIZE[t]*x$AGE_4_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-5,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-5,bt_temp$broodyear),10]=round(x$RUN_SIZE[t]*x$AGE_5_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-6,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-6,bt_temp$broodyear),11]=round(x$RUN_SIZE[t]*x$AGE_6_RETURNS[t])
    }
    if(is.na(match(x$YEAR[t]-7,bt_temp$broodyear))==F){
      bt_temp[match(x$YEAR[t]-7,bt_temp$broodyear),12]=round(x$RUN_SIZE[t]*x$AGE_7_RETURNS[t])
    }
  }
  if(all(is.na(bt_temp[,11])==T)&all(is.na(bt_temp[,12])==T)){
    bt_temp$recruits=apply(bt_temp[,6:10],1,sum)
    
  }else if(all(is.na(bt_temp[,12])==T)){
    bt_temp$recruits=apply(bt_temp[,6:11],1,sum)
  }
  
  sps_bt[[i]]=bt_temp[complete.cases(bt_temp$recruits),]
  s=bt_temp[complete.cases(bt_temp$recruits),]
  s=s[s$recruits/s$spawners<=50,]
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,2]='Chinook'
  stock_dat_temp[,3]=paste(unique(x$stock),'Chinook',sep="-")
  stock_dat_temp[,4]=lat_lons$lat[i] #lat for mouth of Fraser
  stock_dat_temp[,5]=lat_lons$lon[i] #lon for mouth of Fraser
  stock_dat_temp[,6]='Inside WA'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='WA'
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='NWFSC SPS Database, v2022'
  stock_dat_temp[,15]='https://www.webapps.nwfsc.noaa.gov/apex/f?p=261:1::::::' #no comments
  stock_dat_temp[,16]=NA
  stock_dat_temp[,16]='data'
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  chinook_list[[cl+i]]=s[,c('stock','species','broodyear','recruits','spawners','r1','r2','r3','r4','r5','r6','r7')]
}

chinook_filtered = do.call(plyr::rbind.fill, chinook_list)


#Coho####
coho_info$sub.region=gsub('Kuskokwim','AYK',coho_info$sub.region)
coho_info$sub.region=gsub('Southeast','SEAK',coho_info$sub.region)
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
  s_use<- subset(s_use,spawners!=0&recruits!=0)
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=unique(s$stock.id)
  stock_dat_temp[,2]=unique(s$species)
  stock_dat_temp[,3]=paste(unique(s$stock),unique(s$species),sep='-')
  stock_dat_temp[,4]=unique(s_info$lat)
  stock_dat_temp[,5]=unique(s_info$lon)
  stock_dat_temp[,6]=unique(s_info$sub.region)
  stock_dat_temp[,7]=unique(s_info$ocean.region)
  stock_dat_temp[,8]=unique(s_info$jurisdiction)
  
  if(nrow(s_use)!=0){
    stock_dat_temp[,9]=min(s_use$broodyear)
    stock_dat_temp[,10]=max(s_use$broodyear)
    stock_dat_temp[,11]=length(s_use$broodyear)
    stock_dat_temp[,12]=mean(s_use$spawners)/1e3
    stock_dat_temp[,13]=mean(s_use$recruits)/1e3
  }else{
    stock_dat_temp[,9]=NA
    stock_dat_temp[,10]=NA
    stock_dat_temp[,11]=0
    stock_dat_temp[,12:13]=NA
  }
  
  s.id=as.numeric(strsplit(as.character(s_info$source.id),',')[[1]])
  if(length(s.id)==1){
    stock_dat_temp[,14]=coho_source$title[match(s_info$source.id,sockeye_source$source.id)]
    stock_dat_temp[,15]=coho_source$url[match(s_info$source.id,sockeye_source$source.id)]
  }
  if(length(s.id)==2){
    source<- subset(coho_source, source.id %in% s.id)
    stock_dat_temp[,14]=paste(source$source[1],source$source[2],sep='; ')
    stock_dat_temp[,15]=paste(source$url[1],source$url[2],sep='; ')
  }
  stock_dat_temp[,16]=NA #no comments
  stock_dat_temp[,17]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  coho_list[[i]]=s_use[,c('stock','species','broodyear','recruits','spawners','r2','r3','r4','r5','r6','r7')]
}

#Interior Fraser coho brood tables
cl=length(coho_list)
for(i in 1:length(unique(ifr_coho$stock))){
  s=subset(ifr_coho,stock==unique(ifr_coho$stock)[i])
  s$species=rep('Coho',nrow(s))
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=unique(s$stock)
  stock_dat_temp[,2]='Coho'
  stock_dat_temp[,3]=paste(unique(s$stock),'Coho',sep='-')
  stock_dat_temp[,4]=sockeye_info$lat[2] #lat for mouth of Fraser
  stock_dat_temp[,5]=sockeye_info$lon[2] #lon for mouth of Fraser
  stock_dat_temp[,6]='Fraser River'
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  stock_dat_temp[,9]=min(s$broodyear)
  stock_dat_temp[,10]=max(s$broodyear)
  stock_dat_temp[,11]=length(s$broodyear)
  stock_dat_temp[,12]=max(s$spawners)
  stock_dat_temp[,13]=max(s$recruits)
  stock_dat_temp[,14]='Michael Arbeider, DFO, 2022'
  stock_dat_temp[,15]=NA #no comments
  stock_dat_temp[,16]=NA

  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  coho_list[[cl+i]]=s[,c('stock','species','broodyear','recruits','spawners','r3','r4')]
}

#PSE coho series
pse_coho=subset(pse_df,species_name=='Coho')
pse_coho$stock=pse_coho$cu_name_pse

coho_pse_lat=c(54.22,54.22,52.33,52.473,53.31)
coho_pse_lon=c(-129.831,-129.831,-127.185,-128.378,-129.13)
coho_reg=c('North Coast','North Coast','Central Coast','Central COast','Central COast')
cl=length(coho_list)
for(i in 1:length(unique(pse_coho$stock))){
  s=subset(pse_coho,stock==unique(pse_coho$stock)[i])
  s$species='Coho'
  s$broodyear=s$year
  
  stock_dat_temp=data.frame(stock.id=NA,species=NA,stock.name=NA,lat=NA,lon=NA,region=NA,ocean.basin=NA,state=NA,begin=NA,end=NA,n.years=NA,m.spawners=NA,m.recruits=NA,source=NA,url=NA,comments=NA,age=NA)
  
  stock_dat_temp[,1]=NA
  stock_dat_temp[,2]='Coho'
  stock_dat_temp[,3]=paste(unique(s$stock),'Coho',sep="-")
  stock_dat_temp[,4]=coho_pse_lat[i] #lat need to do these (one for Skeena estuary and one for Nass)
  stock_dat_temp[,5]=coho_pse_lon[i] #lon 
  stock_dat_temp[,6]=coho_reg[i]
  stock_dat_temp[,7]='WC'
  stock_dat_temp[,8]='BC'
  
  stock_dat_temp[,9]=min(s$year)
  stock_dat_temp[,10]=max(s$year)
  stock_dat_temp[,11]=length(s$year)
  stock_dat_temp[,12]=mean(s$spawners)/1e3
  stock_dat_temp[,13]=mean(s$recruits)/1e3
  stock_dat_temp[,14]='Salmon Watersheds Program, PSF, 2022'
  stock_dat_temp[,15]=NA
  stock_dat_temp[,17]=NA
  
  stock_dat=rbind(stock_dat,stock_dat_temp)
  
  coho_list[[cl+i]]=s[,c('stock','species','broodyear','recruits','spawners')]
}

coho_filtered = do.call(plyr::rbind.fill,coho_list)



#Print out data####
filtered_productivity_data=full_join(chinook_filtered,chum_filtered) %>% full_join(coho_filtered) %>% full_join(pink_filtered) %>% full_join(sockeye_filtered)
filtered_productivity_data=subset(filtered_productivity_data,spawners>1)

#Stock overview
stock_dat= subset(stock_dat,n.years>9)
stock_dat$stock.id=seq(1:nrow(stock_dat))
stock_dat$stock.name=gsub('/','-',stock_dat$stock.name)
filtered_productivity_data$stock=gsub('/','-',filtered_productivity_data$stock)

filtered_productivity_data$stock=paste(filtered_productivity_data$stock,filtered_productivity_data$species,sep='-')
filtered_productivity_data=subset(filtered_productivity_data,stock %in% stock_dat$stock.name)

filtered_productivity_data1=cbind(filtered_productivity_data[,1:5])
filtered_productivity_data2=filtered_productivity_data[,6:13]
filtered_productivity_data3=filtered_productivity_data[,14:36]

filtered_productivity_data2=filtered_productivity_data2[,order(names(filtered_productivity_data2))]
filtered_productivity_data3=filtered_productivity_data3[,order(names(filtered_productivity_data3))]
filtered_productivity_data=cbind(filtered_productivity_data1,filtered_productivity_data2,filtered_productivity_data3)
filtered_productivity_data$logRS=log(filtered_productivity_data$recruits/filtered_productivity_data$spawners)
filtered_productivity_data$stock.id=stock_dat$stock.id[match(filtered_productivity_data$stock,stock_dat$stock.name)]


#Write datasets
rownames(stock_dat)=NULL
write.csv(filtered_productivity_data,here('data','filtered datasets',paste('salmon_productivity_compilation',Sys.Date(),'.csv',sep='')))
write.csv(stock_dat,here('data','filtered datasets',paste('stock_info',Sys.Date(),'.csv',sep='')))

