library(here);library(dplyr);library(cmdstanr);
library(ggplot2)
`%notin%`<- Negate(`%in%`)
pse=read.csv(here('data','dataset5.csv'))
dqpse=read.csv(here('data','dataset390.csv'))
dqpse2=tidyr::spread(dqpse,key=parameter,value=datavalue)

hdq1=subset(dqpse2,catch_quality>1&survey_coverage>1&survey_execution>1&survey_quality>1)
hdq2=subset(dqpse2,catch_quality>1&survey_coverage>0&survey_execution>1&survey_quality>1)
hdq3=subset(dqpse2,catch_quality>0&survey_coverage>0&survey_execution>0&survey_quality>0)
hdq4=subset(dqpse2,dq_score>8)

pse_chum=subset(hdq4,species=='Chum')
pse_chin=subset(hdq4,species=='Chinook')
pse_chin=subset(pse_chin,regionname!='Skeena') #have alternative skeena datasets for chinook
pse_coho=subset(hdq4,species=='Coho')
pse_soc=subset(hdq4,species=='Lake sockeye'|species=='River sockeye')
pse_soc=subset(pse_soc,regionname %notin% c('Skeena','Fraser')) #have alternative skeena/fraser datasets for sockeye

pse_pink=subset(hdq4,species=='Pink (even)'|species=='Pink (odd)')
hdq4=rbind(pse_chum,pse_chin,pse_coho,pse_pink,pse_soc)


pse_d=subset(pse,cuid %in% hdq4$cuid)
pse_df=subset(pse,spawners>0&recruits>0)
pse_df$stock=paste(pse_df$species_name,pse_df$cu_name_pse)

pse_sum=pse_df%>%group_by(stock) %>% summarize(n=n())


dir.create('outs')

pse=subset(pse,recruits>0&spawners>0)
pse$cu_name_pse=gsub('/','_',pse$cu_name_pse)

cmdstanr::set_cmdstan_path(path='C:/Users/greenbergda/Documents/.cmdstan/cmdstan-2.29.2')

file1=file.path(cmdstanr::cmdstan_path(),'sr models', "m1f_ip2.stan")
m1=cmdstanr::cmdstan_model(file1)

for(i in 1:nrow(hdq4)){
  if(levels(factor(hdq4$cuid))[i] %notin% pse$cuid){next}
  
   s=subset(pse,cuid==levels(factor(hdq4$cuid))[i])
   
   s<- s[complete.cases(s$spawners),]
   
   df=data.frame(S=s$spawners,R=s$recruits,by=s$year)
   df$logRS=log(df$R/df$S)
   #Fit each model
   #model 1 - static Ricker
   dl=list(N=nrow(df),
           L=max(df$by)-min(df$by)+1,
           ii=df$by-min(df$by)+1,
           R_S=df$logRS,
           S=df$S,
           pSmax_mean=0.5*max(df$S),
           pSmax_sig=max(df$S))
   
   f1 = m1$sample(data = dl,
                  seed = 333,
                  chains = 8, 
                  iter_warmup = 200,
                  iter_sampling = 500,
                  refresh = 100,
                  adapt_delta = 0.995,
                  max_treedepth = 20)
   
    d=data.frame(f1$draws(format='draws_matrix'))
   newd=data.frame(S=seq(0,max(df$S)));p=newd$S*exp(median(d$log_a)-median(d$b)*newd$S)
   

   png(filename=paste('./outs/',s$species_name,s$cu_name_pse,'.png',sep=''),width=8,height=6,units='in',res=300)
   plot(R~S,data=df,bty='l',ylab='Recruits',xlab='Spawners',type='n',xlim=c(0,max(df$S)),ylim=c(0,max(df$R)))
   #poly_x=c(s,rev(s))
   #poly_y=c(lims_r[,2],rev(lims_r[,1]))
   #polygon(poly_x, poly_y, col = adjustcolor('dodgerblue4', alpha = 0.4), border=NA) # Add uncertainty polygon
   cols= RColorBrewer::brewer.pal(8,'YlGnBu');
   br=quantile(df$by,probs=seq(0,1,by=0.15))
   col_points=adjustcolor(cols[findInterval(df$by,br)],alpha.f=0.8)
   points(R~S,data=df,pch=21,bg=col_points,cex=1.5)
   lines(p~newd$S,lwd=2)
   for(i in 1:length(br)){text(x=par('usr')[2]-par('usr')[2]*0.1,y=par('usr')[4]-par('usr')[4]*0.05*i,unname(round(br[i])),col=cols[i])}
  abline(c(0,1),col='red')
   dev.off()
   
  }
