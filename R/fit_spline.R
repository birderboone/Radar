fit_spline<-function(top, bottom, data, override=F, data_t=-max(or_spline$y), data_b=min(or_spline$y),nspline=round((max(data$d_time) - min(data$d_time)) * 30), method='fmm'){
  n_spline<-list(x=NULL, y=NULL)
  or_spline<-spline(data$ref ~ data$d_time,n=nspline, method=method)
  bottom_p<-which.min(abs(or_spline$x - bottom))
  top_p<-which.min(abs(or_spline$x - top))
  n_spline$x<-or_spline$x[bottom_p:top_p]
  n_spline$y<-or_spline$y[bottom_p:top_p]  
  date1<-data$date[1]
  if(override==T){data_t<-data_t; data_b<-data_b}
  if(override==F){data_t<-max(or_spline$y); data_b<-min(or_spline$y)}
  ###this requires a top sunangle, bottom sunangle, and a spline function
  #plot(new ~ n_spline$x)
  #groupd<-data.frame(ref=n_spline$y, sunangle=n_spline$x)
  ##############################################
  #groupd<-groupedData(ref~sunangle|date, data=my.data)
  #groupd<-groupd[order(groupd$sunangle),]
  # ##########trouble shooting options#########
  # groupd<-groupd[0:(nrow(groupd)-7),]   ###for cropping data
  # groupd<-groupd[(-10),]                ###too delete a point
  # groupd$ref[6]<-(groupd$ref[6]+(1.1)) ###to move a point
  #plot(groupd$sunangle, groupd$ref)    ### to test if the fixes were appropriate
  ###########################################
  #plot(n_spline$y ~ n_spline$x)
  
  diff<-n_spline$y[2:length(n_spline$y)] - n_spline$y[1:(length(n_spline$y)-1)]
  inf<-which.max(diff)
  #start_v<-min(n_spline$y) +(n_spline$y[inf] - min(n_spline$y))*.
  #start<-max(which(diff[1:inf]<0)) +2
  tt<-min(which(diff[inf:1]<0))
  if(is.infinite(tt)){tt<-(inf-1)}
  start<-inf-tt
  if(start==0){start<-1}
  #start<-inf-
  start_v<-(n_spline$y[inf] - n_spline$y[start]) *0.05 + n_spline$y[start]
  
  start_v1<-which(n_spline$x==n_spline$x[start:inf][which.min(abs( n_spline$y[start:inf]-start_v))])
  # start_v<-n_spline$y[which.max(which(diff[1:inf]<0))] *1.05
  # which.max(which(diff[1:inf]<0)):inf
  # start<-which.min(abs(n_spline$y -start_v))
  #abline(v=n_spline$x[inf],col='green')
  #abline(v=n_spline$x[start_v1],col='black')
  
  plot(or_spline$y ~ or_spline$x, ylim=c(data_b, data_t), ylab='reflectivity', xlab='decimal hour')
  abline(v=n_spline$x[inf],col='green')
  abline(v=n_spline$x[start_v1],col='black')
  
  points(data$ref ~ data$d_time, col='red')
  title(main=paste0(names(ref_data)[[pp]],'_(',pp,')'))
  #legend('bottomright', paste0(signif(n_spline$x[inf],4)),col=c('black'))
  
 
  ####calculate sunset piecewise
  approx_sun<-floor(suncalc(as.POSIXlt(paste0(date1,' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC')$yday, lat, long, UTC=F)$sunset + 4) 
  tt1<-as.POSIXlt((as.POSIXlt(paste0(date1,' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3)[which.min(abs(SZA((as.POSIXlt(paste0(date1,' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3),lat,long)-90))],format='%Y-%m-%d %H:%M:%S')
  
  abline(v=to_dhour(tt1),col='red')
  
 pars<<- list(start=n_spline$x[start_v1],
  inflection=n_spline$x[inf],
  sunset=to_dhour(tt1),strength=n_spline$y[inf]-n_spline$y[start])
  
}