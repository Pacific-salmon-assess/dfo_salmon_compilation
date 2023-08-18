#functions
library(wesanderson);library(RColorBrewer);library(cowplot)
#Colour assignment function - x - vector of values (broodyear here), cols = palette of choice, breaks - set the division ie. 1/10 (0.1); 1/5 (0.2)
`%notin%`<- Negate(`%in%`)

whichColour<-function(x,cols,breaks){
  b=quantile(x,probs=seq(0,1,by=breaks)) 
  i<-1
  while(x>=b[i]&&x>b[i+1]) i<-i+1
  cols[i]
}

assignCol<- function(x,cols,breaks){
  sapply(x$broodyear,whichColour,cols=cols,breaks=breaks)
}

#Plot function - recruit per spawner & SR plot by time
plot_SR=function(x,m,path){ #x = dataset, m = S-R model fit, path = desired output folder
  x_new<- seq(min(x$spawners),max(x$spawners))
  pred_r<- exp(m$coefficients[1]+m$coefficients[2]*x_new)*x_new
  a<- exp(m$coefficients[1]);b=-m$coefficients[2]
  rmax<- a/b*exp(-1)

  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=cols[findInterval(x$broodyear,br)]
  col_points2=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)

 pdf(file.path(path,paste(paste(x$species,gsub('/','_',x$stock),x$stock.id,sep='_'),'.pdf',sep='')),width=14,height=7)
 par(mfrow=c(1,2))
 plot(exp(logR_S)~broodyear,data=x,bty='l',ylab='Recruits per Spawner',xlab='Year',type='n',cex.axis=1.2,cex.lab=1.2)
 lines(exp(logR_S)~broodyear,lwd=2,data=x)
 points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
 mtext(paste(unique(x$stock),unique(x$species),sep=' '),side=3,font=2,adj=1.5,line=2,cex=1.5)
 
  plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
  lines(pred_r~x_new,lwd=2)
  lines(c(0,rmax)~c(1/b,1/b),lty=5)
  points(recruits~spawners,data=x,bg=col_points2,cex=1.7,pch=21,col='white')
  
  #Legend##
  legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
  rasterImage(legend_image, 0, 0, 1,1)
  op <- par(  ## set and store par
    fig=c(0.9,0.99, 0.7, 0.95),    ## set figure region , 
    mar=c(1, 1, 1, 3),                                  ## set margins
    new=TRUE)                                ## set new for overplot w/ next plot
  
  plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
  rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
  lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
  mtext(round(seq(min(s$broodyear),max(s$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
  mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
  ##
  dev.off()  ## reset par

}

#Plot SR stan
#Plot function - recruit per spawner & SR plot by time
plot_SR=function(x,params,mod,path){ #x = dataset, m = S-R model fit, path = desired output folder
  x_new<- seq(min(x$S),max(x$S))
  
  pred_r<- matrix(nrow=nrow(params),ncol=x$N)
  for(i in 1:nrow(pred_r)){
    pred_r[i,]=exp(params$log_a[i]-params$b[i]*x$S)
  }
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=cols[findInterval(x$broodyear,br)]
  col_points2=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  
  pdf(file.path(path,paste(paste(x$species,gsub('/','_',x$stock),x$stock.id,sep='_'),'.pdf',sep='')),width=14,height=7)
  par(mfrow=c(1,2))
  plot(exp(logR_S)~broodyear,data=x,bty='l',ylab='Recruits per Spawner',xlab='Year',type='n',cex.axis=1.2,cex.lab=1.2)
  lines(exp(logR_S)~broodyear,lwd=2,data=x)
  points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
  mtext(paste(unique(x$stock),unique(x$species),sep=' '),side=3,font=2,adj=1.5,line=2,cex=1.5)
  
  plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
  lines(pred_r~x_new,lwd=2)
  lines(c(0,rmax)~c(1/b,1/b),lty=5)
  points(recruits~spawners,data=x,bg=col_points2,cex=1.7,pch=21,col='white')
  
  #Legend##
  legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
  rasterImage(legend_image, 0, 0, 1,1)
  op <- par(  ## set and store par
    fig=c(0.9,0.99, 0.7, 0.95),    ## set figure region , 
    mar=c(1, 1, 1, 3),                                  ## set margins
    new=TRUE)                                ## set new for overplot w/ next plot
  
  plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
  rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
  lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
  mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
  mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
  ##
  dev.off()  ## reset par
  
}

#plot resid trend
plot_resid_t<- function(x,m.col,l95.col,u95.col,sp){
  plot(c(1:nrow(x))~rep(1,nrow(x)),xlim=c(min(x[,l95.col]),max(x[,u95.col])),xlab='Residual variance trend',ylab='',bty='n',type='n',yaxt='n',main=sp)
  abline(v=0,lty=5)
  for(i in 1:nrow(x)){
    lines(rep(i,2)~c(x[i,l95.col],x[i,u95.col]),lwd=2)
    points(i~x[i,m.col],pch=21,cex=1.2,bg='black')
    par(xpd=T)
    text(y=i,x=par("usr")[1],x$stock[i],cex=1,pos=3,xpd=T,font=1)
  }
}


#plot acf
plot_acf<- function(x,l95.col,u95.col,sp){
  plot(c(1:nrow(x))~rep(1,nrow(x)),xlim=c(-1,1),xlab='Autocorrelation [t-1]',ylab='',bty='l',type='n',yaxt='n',main=sp)
  abline(v=0,lty=5)
  for(i in 1:nrow(x)){
    lines(rep(i,2)~c(x[i,l95.col],x[i,u95.col]),lwd=2)
    points(i~x$ar1[i],pch=21,cex=1.2,bg='black')
  }
}

#plot Stan output of SR
plot_SR_stan=function(x,m,params,pdf){ #x = dataset, m = S-R model fit, path = desired output folder
  x_new<- seq(min(x$spawners),max(x$spawners),by=(max(x$spawners)-min(x$spawners))/200)
  
  pred_r<- matrix(nrow=nrow(params),ncol=length(x_new))
  for(i in 1:nrow(pred_r)){
    pred_r[i,]=exp(params$log_a[i]-params$b[i]*x_new)*x_new
  }
  pred_m=exp(median(params$log_a)-median(params$b)*x_new)*x_new
  rmax<- exp(median(params$log_a))/median(params$b)*exp(-1)
  
  
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=cols[findInterval(x$broodyear,br)]
  col_points2=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  
  if(pdf==1){
    pdf(file.path(path,paste(paste(x$species,gsub('/','_',x$stock),x$stock.id,sep='_'),'.pdf',sep='')),width=14,height=7)
  }
  par(mfrow=c(1,2))
  plot(exp(logR_S)~broodyear,data=x,bty='l',ylab='Recruits per Spawner',xlab='Year',type='n',cex.axis=1.2,cex.lab=1.2)
  lines(exp(logR_S)~broodyear,lwd=2,data=x)
  points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
  mtext(paste(unique(x$stock),unique(x$species),sep=' '),side=3,font=2,adj=1.5,line=2,cex=1.5)
  
  plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
  for(i in 1:2000){
    lines(pred_r[sample(nrow(pred_r),1),]~x_new,col=adjustcolor('darkgray',alpha.f=0.02))
  }
  lines(pred_m~x_new,lwd=2)
  lines(c(0,rmax)~c(1/median(params$b),1/median(params$b)),lty=5)
  points(recruits~spawners,data=x,bg=col_points2,cex=1.7,pch=21,col='white')
  
  #Legend##
  legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
  rasterImage(legend_image, 0, 0, 1,1)
  op <- par(  ## set and store par
    fig=c(0.9,0.99, 0.7, 0.95),    ## set figure region , 
    mar=c(1, 1, 1, 3),                                  ## set margins
    new=TRUE)                                ## set new for overplot w/ next plot
  
  plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
  rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
  lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
  mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
  mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
  ##
  dev.off()  ## reset par
  
}

#plot params
plot_tv_params_comp=function(params,params_gp,type=0,pdf=0,x,path=here('outputs','initial stan runs','sockeye','TV parameter plots')){
  if(type==1){
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=6)
    for(i in 1:ncol(log_a1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_productivity',sep='_','.pdf')),width=11,height=8.5)
    }
    par(mfrow=c(2,1))
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main=paste('Productivity (R/S) -',x$stock[i],x$species[i],sep=' '))
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Productivity - GP estimate')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    if(pdf==1){
      dev.off()
    }
  }
  if(type==2){
    b1= as.data.frame(params[,grepl('log_b',colnames(params))])
    b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    b_mat=matrix(ncol=ncol(b1),nrow=6)
    for(i in 1:ncol(b1)){
      b_mat[1,i]=log10(1/median(exp(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.05))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.95))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.05))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.95))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_capacity',sep='_','.pdf')),width=14,height=8.5)
    }
    par(mfrow=c(2,1))
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='',main=paste('Smax (1/b) -',x$stock[i],x$species[i],sep=' '),yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    
    abline(h=0,lty=3)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='Year',main='Smax (1/b) - GP estimate',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    if(pdf==1){
      dev.off()
    }
  }
  if(type==3){
  log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
  b1= as.data.frame(params[,grepl('log_b',colnames(params))])
  log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
  b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=6)
    b_mat=matrix(ncol=ncol(b1),nrow=6)
     for(i in 1:ncol(b1)){
       a_mat[1,i]=exp(median(log_a1[,i]))
       a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
       a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
       b_mat[1,i]=log10(1/exp(median(b1[,i])))
       b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.05))
       b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.95))
       a_mat[4,i]=exp(median(log_a2[,i]))
       a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
       a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
       b_mat[4,i]=log10(1/median(exp(b2[,i])))
       b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.05))
       b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.95))
     }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_prod_cap',sep='_','.pdf')),width=11,height=8.5)
    }
    par(mfrow=c(2,2))
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main=paste('Productivity (R/S) -',x$stock[i],x$species[i],sep=' '))
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='',main=paste('Smax (1/b) -',x$stock[i],x$species[i],sep=' '),yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Productivity - GP estimate')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
     plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='Year',main='Smax (1/b) - GP estimate',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    if(pdf==1){
      dev.off()
    }
}
  
}


rw_gp_dlm_plot_comp=function(params,params_gp,params_dlm,type=0,pdf=0,x,path=here('outputs','initial stan runs','sockeye','TV parameter plots')){
#  dev.off()
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  l=lm(x$logR_S~x$spawners)
  
  if(type==1){
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    log_a3= as.data.frame(params_dlm[,grepl('alpha',colnames(params_dlm))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=9)
    for(i in 1:ncol(log_a1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
      a_mat[7,i]=exp(log_a3[i,1])
      a_mat[8,i]=exp(log_a3[i,1]-log_a3[i,2]*2)
      a_mat[9,i]=exp(log_a3[i,1]+log_a3[i,2]*2)
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_productivity',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(3,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
        ##
   # plot(exp(x$logR_S)~x$broodyear,type='l',lwd=2,bty='l',ylab='Productivity (R/S)',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='')
    #points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main='Max Productivity - Stan RW')
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - Stan GP')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - DLM')
    abline(h=1,lty=3)
    lines(a_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[9,]~x$broodyear,lwd=2,lty=5)
    
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
    
    if(pdf==1){
      dev.off()
    }
  }
  if(type==2){
    b1= as.data.frame(params[,grepl('log_b',colnames(params))])
    b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    b3= as.data.frame(params_dlm[,grepl('beta',colnames(params_dlm))])
    b_mat=matrix(ncol=ncol(b1),nrow=9)
    for(i in 1:ncol(b1)){
      b_mat[1,i]=log10(1/median(exp(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.05))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.95))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.05))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.95))
      b_mat[7,i]=log10(1/-b3[i,1])
      b_mat[8,i]=log10(1/-(b3[i,1]-2*b3[i,2]))
      b_mat[9,i]=log10(1/-(b3[i,1]+2*b3[i,2]))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_capacity',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(3,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    ##
    # plot(exp(x$logR_S)~x$broodyear,type='l',lwd=2,bty='l',ylab='Productivity (R/S)',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='')
    #points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='',main='Smax - Stan RW',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    abline(h=1,lty=3)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='Year',main='Smax - Stan GP',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    abline(h=1,lty=3)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='Year',main='Smax - DLM',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    abline(h=1,lty=3)
    lines(b_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[9,]~x$broodyear,lwd=2,lty=5)
    
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
    if(pdf==1){
      dev.off()
    }
  }
  if(type==3){
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    b1= as.data.frame(params[,grepl('log_b',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    log_a3= as.data.frame(params_dlm[,grepl('alpha',colnames(params_dlm))])
    b3= as.data.frame(params_dlm[,grepl('beta',colnames(params_dlm))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=9)
    b_mat=matrix(ncol=ncol(b1),nrow=9)
    for(i in 1:ncol(b1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.025))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.975))
      b_mat[1,i]=log10(1/exp(median(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.025))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.975))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.025))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.975))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.025))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.975))
      a_mat[7,i]=exp(log_a3[i,1])
      a_mat[8,i]=exp(log_a3[i,1]-log_a3[i,2]*2)
      a_mat[9,i]=exp(log_a3[i,1]+log_a3[i,2]*2)
      b_mat[7,i]=log10(1/-b3[i,1])
      b_mat[8,i]=log10(1/-(b3[i,1]-2*b3[i,2]))
      b_mat[9,i]=log10(1/-(b3[i,1]+2*b3[i,2]))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_prod_cap',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(4,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main='Max Productivity - Stan RW')
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - Stan GP')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - DLM')
    abline(h=1,lty=3)
    lines(a_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[9,]~x$broodyear,lwd=2,lty=5)
    
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='',main='Smax - Stan RW',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    abline(h=1,lty=3)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='Year',main='Smax - Stan GP',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    abline(h=1,lty=3)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='Year',main='Smax - DLM',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    abline(h=1,lty=3)
    lines(b_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[9,]~x$broodyear,lwd=2,lty=5)
    
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
 #   dev.off()
    if(pdf==1){
      dev.off()
    }
  }
  
}

rw_gp_dlm_ou_plot_comp=function(params,params_gp,params_dlm,params_ou,pdf=0,x,path=here('outputs','initial stan runs','sockeye','TV parameter plots')){
  #  dev.off()
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  l=lm(x$logR_S~x$spawners)
  
 
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    log_a3= as.data.frame(params_dlm[,grepl('alpha',colnames(params_dlm))])
    log_a4= as.data.frame(params_ou[,grepl('log_a',colnames(params_ou))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=12)
    for(i in 1:ncol(log_a1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
      a_mat[7,i]=exp(log_a3[i,1])
      a_mat[8,i]=exp(log_a3[i,1]-log_a3[i,2]*2)
      a_mat[9,i]=exp(log_a3[i,1]+log_a3[i,2]*2)
      a_mat[10,i]=exp(median(log_a4[,i]))
      a_mat[11,i]=exp(quantile(log_a4[,i],0.05))
      a_mat[12,i]=exp(quantile(log_a4[,i],0.95))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_productivity',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(3,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    ##
    # plot(exp(x$logR_S)~x$broodyear,type='l',lwd=2,bty='l',ylab='Productivity (R/S)',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='')
    #points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main='Max Productivity - Stan RW')
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - Stan GP')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - DLM')
    abline(h=1,lty=3)
    lines(a_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[9,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[10,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - DLM')
    abline(h=1,lty=3)
    lines(a_mat[11,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[12,]~x$broodyear,lwd=2,lty=5)
    
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
    
    if(pdf==1){
      dev.off()
    }
}

prod_cap_corr_comp=function(params1,params2,pdf=0,x,path=here('outputs','initial stan runs','sockeye','TV parameter plots')){
  #  dev.off()
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  l=lm(x$logR_S~x$spawners)
  
    log_a1= as.data.frame(params1[,grepl('log_a',colnames(params1))])
    b1= as.data.frame(params1[,grepl('log_b',colnames(params1))])
    log_a2= as.data.frame(params2[,grepl('log_a',colnames(params2))])
    b2= as.data.frame(params2[,grepl('log_b',colnames(params2))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=6)
    b_mat=matrix(ncol=ncol(b1),nrow=6)
    for(i in 1:ncol(b1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.025))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.975))
      b_mat[1,i]=log10(1/exp(median(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.025))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.975))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.025))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.975))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.025))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.975))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'Ind_corr_RW',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(4,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main='Max Productivity - ind RW')
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - cor RW')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='',main='Smax - ind RW',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    abline(h=1,lty=3)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(na.omit(b_mat))*0.95,max(na.omit(b_mat))*1.05),ylab='',xlab='Year',main='Smax - cor RW',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    abline(h=1,lty=3)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    
    hist(params2$`Cor_1[1,2]`,main='Correlation a & b',xlab='r')
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
    #   dev.off()
    if(pdf==1){
      dev.off()
    }
  }
  


rw_ab_plot_comp=function(params,params_gp,params_dlm,type=0,pdf=0,x,path=here('outputs','initial stan runs','sockeye','TV parameter plots')){
  
  cols=wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")
  
  br=quantile(x$broodyear,probs=seq(0,1,by=0.02))
  col_points1=adjustcolor(cols[findInterval(x$broodyear,br)],alpha.f=0.8)
  l=lm(x$logR_S~x$spawners)
  
  if(type==1){
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    log_a3= as.data.frame(params_dlm[,grepl('alpha',colnames(params_dlm))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=9)
    for(i in 1:ncol(log_a1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
      a_mat[7,i]=exp(log_a3[i,1])
      a_mat[8,i]=exp(log_a3[i,1]-log_a3[i,2]*2)
      a_mat[9,i]=exp(log_a3[i,1]+log_a3[i,2]*2)
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_productivity',sep='_','.pdf')),width=10,height=14)
    }
    par(mfrow=c(3,2))
    plot(recruits~spawners,data=x,bty='l',ylab='Recruits (R)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2,main=paste(x$stock[i],x$species[i],sep=' '))
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))*seq(min(x$spawners),max(x$spawners),length.out=100)~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    lines(c(0,l$coefficients[1]/-l$coefficients[2]*exp(-1))~c(1/-l$coefficients[2],1/-l$coefficients[2]),lty=5)
    points(recruits~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(exp(logR_S)~spawners,data=x,bty='l',ylab='Productivity (R/S)',xlab='Spawners (S)',type='n',cex.axis=1.2,cex.lab=1.2)
    lines(exp(l$coefficients[1]+l$coefficients[2]*seq(min(x$spawners),max(x$spawners),length.out=100))~seq(min(x$spawners),max(x$spawners),length.out=100),lwd=2)
    points(exp(logR_S)~spawners,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    ##
    # plot(exp(x$logR_S)~x$broodyear,type='l',lwd=2,bty='l',ylab='Productivity (R/S)',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='')
    #points(exp(logR_S)~broodyear,data=x,bg=col_points1,cex=1.7,pch=21,col='white')
    
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main='Max Productivity - Stan RW')
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - Stan GP')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(a_mat[7,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Max Productivity - DLM')
    abline(h=1,lty=3)
    lines(a_mat[8,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[9,]~x$broodyear,lwd=2,lty=5)
    
    
    legend_image <- as.raster(matrix(rev(wes_palette("Zissou1",length(seq(0,1,by=0.02)), type="continuous")), ncol=1))
    rasterImage(legend_image, 0, 0, 1,1)
    op <- par(  ## set and store par
      fig=c(0.9,0.99, 0.85, 0.95),    ## set figure region , 
      mar=c(1, 1, 1, 3),                                  ## set margins
      new=TRUE)                                ## set new for overplot w/ next plot
    
    plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')  ## ini plot2
    rasterImage(legend_image, 0, 0, 1, 1)                       ## the gradient
    lbsq <- seq.int(0, 1, l=5)                                  ## seq. for labels ## axis ticks
    mtext(round(seq(min(x$broodyear),max(x$broodyear),l=5)), 4, -.5, at=lbsq, las=2, cex=.6,font=2)                      ## tick labels
    mtext('Year', 3, 0.125, cex=.8, adj=-0.1, font=2)              ## title
    
    if(pdf==1){
      dev.off()
    }
  }
  if(type==2){
    b1= as.data.frame(params[,grepl('log_b',colnames(params))])
    b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    b3= as.data.frame(params_dlm[,grepl('alpha',colnames(params_dlm))])
    b_mat=matrix(ncol=ncol(b1),nrow=9)
    for(i in 1:ncol(b1)){
      b_mat[1,i]=log10(1/median(exp(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.05))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.95))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.05))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.95))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_capacity',sep='_','.pdf')),width=14,height=8.5)
    }
    par(mfrow=c(2,1))
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='',main=paste('Smax (1/b) -',x$stock[i],x$species[i],sep=' '),yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    
    abline(h=0,lty=3)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='Year',main='Smax (1/b) - GP estimate',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    if(pdf==1){
      dev.off()
    }
  }
  if(type==3){
    log_a1= as.data.frame(params[,grepl('log_a',colnames(params))])
    b1= as.data.frame(params[,grepl('log_b',colnames(params))])
    log_a2= as.data.frame(params_gp[,grepl('log_a',colnames(params_gp))])
    b2= as.data.frame(params_gp[,grepl('log_b',colnames(params_gp))])
    a_mat=matrix(ncol=ncol(log_a1),nrow=6)
    b_mat=matrix(ncol=ncol(b1),nrow=6)
    for(i in 1:ncol(b1)){
      a_mat[1,i]=exp(median(log_a1[,i]))
      a_mat[2,i]=exp(quantile(log_a1[,i],0.05))
      a_mat[3,i]=exp(quantile(log_a1[,i],0.95))
      b_mat[1,i]=log10(1/exp(median(b1[,i])))
      b_mat[2,i]=log10(1/quantile(exp(b1[,i]),0.05))
      b_mat[3,i]=log10(1/quantile(exp(b1[,i]),0.95))
      a_mat[4,i]=exp(median(log_a2[,i]))
      a_mat[5,i]=exp(quantile(log_a2[,i],0.05))
      a_mat[6,i]=exp(quantile(log_a2[,i],0.95))
      b_mat[4,i]=log10(1/median(exp(b2[,i])))
      b_mat[5,i]=log10(1/quantile(exp(b2[,i]),0.05))
      b_mat[6,i]=log10(1/quantile(exp(b2[,i]),0.95))
    }
    
    if(pdf==1){
      pdf(file.path(path,paste(x$stock.id,x$stock[i],x$species[i],'TV_prod_cap',sep='_','.pdf')),width=11,height=8.5)
    }
    par(mfrow=c(2,2))
    plot(a_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='',main=paste('Productivity (R/S) -',x$stock[i],x$species[i],sep=' '))
    abline(h=1,lty=3)
    lines(a_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[3,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[1,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='',main=paste('Smax (1/b) -',x$stock[i],x$species[i],sep=' '),yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[2,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[3,]~x$broodyear,lwd=2,lty=5)
    
    plot(a_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(a_mat)*0.95,max(a_mat)*1.05),ylab='',xlab='Year',main='Productivity - GP estimate')
    abline(h=1,lty=3)
    lines(a_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(a_mat[6,]~x$broodyear,lwd=2,lty=5)
    plot(b_mat[4,]~x$broodyear,type='l',lwd=2,bty='l',ylim=c(min(b_mat)*0.95,max(b_mat)*1.05),ylab='',xlab='Year',main='Smax (1/b) - GP estimate',yaxt='n')
    axis(2, col="black", at=seq(1,10,by=1),   tcl=-0.45, cex.axis=1.1,
         labels=c(expression(10),expression(100),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7),expression(10^8),expression(10^9),expression(10^10)))
    pow <- 1:10
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, log10(ticksat), col="black", labels=NA,
         tcl=-0.2, lwd=0, lwd.ticks=1)
    lines(b_mat[5,]~x$broodyear,lwd=2,lty=5)
    lines(b_mat[6,]~x$broodyear,lwd=2,lty=5)
    if(pdf==1){
      dev.off()
    }
  }
  
}


#plot posterior predictive check - y = observed, y_rep = MCMC draws, samps = # of samples
plot_ppd=function(y,y_rep,samps){
  y_rep_sub=y_rep[sample(nrow(y_rep),samps)]
  plot(density(y,bw=0.05),bty='l')
  for(i in 1:length(samps)){
    
  }
  
}

#' Optimize sGen
#'
#' This is a component function within sGenSolver that estimates the log-
#' likelihood of the relevant Ricker function.
#'
#' @param S A numeric of spawner abundances.
#' @param theta A numeric vector of Ricker stock recruit parameters: alpha,
#' beta, and sigma.
#' @param sMSY A numeric of spawner abundance estimated to result in maximum
#' sustainable yield.
#' @return Returns a list of prt (estimated productivity), epsilon (the
#' difference between sMSY and productivity), the negative log likelihood
#' used to estimate sGen, and S.
#'
#' @export

sGenOptimum <- function(S, theta, sMSY) {
  a = theta[1]
  b = theta[2]
  sig = exp(theta[3])
  prt <- S * exp(a - b * S)
  epsilon <- log(sMSY) - log(prt)
  nLogLike <- sum(dnorm(epsilon, 0, sig, log = T))
  
  return(list(prt = prt, epsilon = epsilon, nLogLike = nLogLike, S = S))
}

#______________________________________________________________________________

#' Solve for sGen
#'
#' This function solves for sGen based on sMSY and the log-likelihood estimated
#' in sGenOptimum.
#'
#' @param theta A numeric vector of Ricker stock recruit parameters: alpha,
#' beta, and sigma.
#' @param sMSY A numeric of spawner abundance estimated to result in maximum
#' sustainable yield.
#' @return Returns a numeric that is the spawner abundance that minimizes the
#' the log likelihood.
#'
#' @examples
#' Stock-recruit parameters approximate those of Fraser River sockeye salmon
#' Chilko CU.
#' alpha = 1.2
#' beta = 1.5
#' sigma = 0.8
#' theta = c(alpha, beta, sigma)
#' sMSY = 0.3
#' sGenSolver(theta, sMSY)
#' @export

sGenSolver <- function(theta, sMSY) {
  #gives the min Ricker log-likelihood
  fnSGen <- function(S, theta, sMSY) -1.0 * sGenOptimum(S, theta, sMSY)$nLogLike
  fit <- optimize(f = fnSGen, interval = c(0, ((theta[1] / theta[2]) * (0.5 - 0.07 * theta[1]))),
                  theta = theta, sMSY = sMSY)
  return(list(fit = fit$minimum))
}