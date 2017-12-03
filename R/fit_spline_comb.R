# ### troubleshooting
# data=my.data
# override=F
# data_t=-max(or_spline$y)
# data_b=min(or_spline$y)
# nspline=round((max(data$d_time) - min(data$d_time)) * 30)
# method='fmm'
# per_rate=.2
# max_min=15
#' Fits a single exodus curve using the combined method
#' 
#' Prints out the curve, sunset, and what time was fit based on user specific criteria
#' @param start_t Approximate start time
#' @param data A data.frame with two columns: d_time = decimal time and ref = reflectivity.
#' @param nspline Number of spline points, defaults to 2 minute intervals
#' @param method The method for spline fitting. This doesn't change fitting much
#' @param max_min Maximum minutes to consider for curve fitting
#' @param per_rate Percentage of change in reflectivity to consider the start of exodus
#' 
fit_spline_comb<-function(start_t, data,nspline=round((max(data$d_time) - min(data$d_time)) * 30), method='natural',per_rate=.2,max_min=15){
  n_spline<-list(x=NULL, y=NULL)
  or_spline<-spline(data$ref ~ data$d_time,n=nspline, method=method)
  n_spline<-or_spline
  date1<-data$date[1]
  #if(override==T){data_t<-data_t; data_b<-data_b}
  #if(override==F){data_t<-max(or_spline$y); data_b<-min(or_spline$y)}
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
  ########################
  ##run some logic to figure out which points to choose between
  #plot(n_spline)
  #plot(diff)
  diff_2nd<-diff[2:length(diff)]-diff[1:(length(diff)-1)]
  #plot(diff_2nd)
  
  ###Logic begin
  #if(sum(diff_2nd>0)==length(diff_2nd)){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points'))}
  #if(!is.finite(infl)){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points'))}
  
  #infl<-which(diff_2nd[1:(length(diff_2nd)-1)]>0 & diff_2nd[2:length(diff_2nd)]<0 & (angle_spline$x>=92)[2:(length(angle_spline$x)-2)] )[1] +1
  #if(!is.finite(infl)){infl<-length(n_spline$x)}
  
  start<-which(n_spline$x==incomp(n_spline$x,start_t))
  
  
  if(!is.finite(start)){start<-1}
  #if(diff[start+1]<(max(diff)*per_rate)){start<-(which(diff[2:length(diff)]>(max(diff)*per_rate) & (2:length(diff))>=start) +1)[1]}
  
  #if(!is.finite(start)){print(paste0(names(here)[[pp]],'_(',pp,'): needs to be fit manually')); next}
  
  ##recalculate the inflection with the new start point established
  infl<-which(diff_2nd[1:(length(diff_2nd)-1)]>0 & diff_2nd[2:length(diff_2nd)]<0 & n_spline$x[2:(length(n_spline$x)-2)]>=n_spline$x[2:(length(n_spline$x)-2)][start] & (n_spline$x<(n_spline$x[start]+.75))[2:(length(n_spline$x)-2)])
  
  
  if(length(infl)<1||!is.finite(infl)){infl<-length(n_spline$x)}
  infl<-infl[which.max(diff[infl-1])]
  if(infl<length(n_spline$x)){infl<-infl+1}
  
  #if(!is.finite(infl) & (sum(n_spline$x>(n_spline$x[start] +  max_min/60))==0 )){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points'))}
  
  #if(infl<start){print(paste0(names(here)[[pp]],'_(',pp,'): needs to be manually fit'))}
  
  #if(n_spline$y[infl]<1){print(paste0(names(here)[[pp]],'_(',pp,'): inflection point was below 1 Z. Check night, then manually fit'))}
  
  
  
  n_spline$x[infl]
  n_spline$x[start] +  max_min/60
  choice<-which.min(c(n_spline$x[infl], n_spline$x[start] +  max_min/60))
  infl_final<-min(c(n_spline$x[infl], n_spline$x[start] +  max_min/60),na.rm=T)
  infl_final<-which(n_spline$x==incomp(n_spline$x,infl_final))
  plot(or_spline$y ~ or_spline$x)
  title(main=paste0(date1,'_(',pp,')'))
  abline(v=n_spline$x[infl],col='green')
  abline(v=n_spline$x[start],col='black')
  abline(v=(n_spline$x[start]+max_min/60), col='blue')
  
  points(my.data$ref ~ my.data$d_time, col='red')
  #legend('bottomright', paste0(signif(n_spline$x[inf],4)),col=c('black'))

  ####calculate sunset piecewise
  approx_sun<-floor(suncalc(as.POSIXlt(paste0(answerframe$date[pp],' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC')$yday, lat, long, UTC=F)$sunset + 4) 
  tt1<-as.POSIXlt((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3)[which.min(abs(SZA((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3),lat,long)-90))],format='%Y-%m-%d %H:%M:%S')
  
  abline(v=to_dhour(tt1),col='red')
  
  warning(paste0('red = sunset, black = start of exodus, green = time of maximum increase, blue = ',max_min,' minutes after start of exodus'),call.=F)
  
  pars<<- list(start=n_spline$x[start],
               inflection=n_spline$x[infl],
               sunset=to_dhour(tt1),
               strength=n_spline$y[infl_final]-n_spline$y[start],
               method=c('inflection','start')[choice],
               sample=n_spline$x[infl_final])
}