# ###troubleshooting
# exrad_n="V:/Documents/nexrad_site_list_with_utm.csv"
# out_netcdf
# radar
# max_min=15
#' Fits exodus curves using the combined method for a series of files
#' 
#' @param out_netcdf location of reflectivity file created from exodusangle_netcdf
#' @param radar 3 letter radar code
#' @param max_min maximum minutes to consider for curve fitting
#' @param per_rate percentage of change in reflectivity to consider the start of exodus
#' @param nexrad_n  location of the nexrad site table. Default is the location on the AP's server

exodus_fit_spline_comb<-function(out_netcdf, radar,max_min=15,per_rate=.15,nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv" ,tol=1){
  require(RAtmosphere)
  require(nlme)
  nexrad_get(radar=radar,nexrad_n=nexrad_n)

  final<-read.csv(out_netcdf)
  final<-na.omit(final)
  times<-as.POSIXlt(final$times,format='%Y-%m-%d %H:%M')
  final$d_time<-to_dhour(times)
  final$d_time[final$d_time<12]<-final$d_time[final$d_time<12] + 24
  final$sunangle<-SZA(final$times, Lat=lat, Lon=long)
  final$ref<-final$ref+abs(min(final$ref))
  here<-by(final,list(final$date),data.frame)
  
  answerframe<-as.data.frame(matrix(ncol=7, nrow=length(here)))
    colnames(answerframe)<-c('date','start','inflection','sunset','strength','method','sample')
 answerframe$date<-names(here)
 error<-list()
  #final<-final[order(final$times),]
  ################################################
  ###TAKE A BREAK SON
  ################################################
  ##changes plot window
  par( mfrow = c( 4, 4 ) )
  par(mar=c(2,2,1.5,1))
  ################################################
  for(pp in 1:dim(here)){ 
    #for(pp in 106:110){ 
    #pp<-12
    my.data<-here[[pp]]
    my.data<-my.data[order(my.data$d_time),]
    nspline<-round((max(my.data$d_time) - min(my.data$d_time)) * 30)
    groupd<-groupedData(ref~d_time|date, data=my.data)
    groupd<-groupd[order(groupd$sunangle),]
    groupd<-groupd[groupd$sunangle>87 & groupd$sunangle<110,]
    
    
    if(dim(groupd)[1]<5) {print(paste0(names(here)[[pp]],'_(',pp,') has too few observations')); next}
    
    n_spline<-spline(groupd$d_time, groupd$ref,n=nspline)
    angle_spline<-spline(groupd$sunangle, groupd$ref,n=nspline)
    or_spline<-n_spline
    angle_range<-which(spline(groupd$sunangle, groupd$ref,n=nspline)$x<=110 & spline(groupd$sunangle, groupd$ref,n=nspline)$x>=90) 
    if(length(angle_range)<2){print(paste0(names(here)[[pp]],'_(',pp,'): not enough points below 100')); next}
    n_spline$y<-n_spline$y[angle_range]
    n_spline$x<-n_spline$x[angle_range]
    angle_spline$y<-angle_spline$y[angle_range]
    angle_spline$x<-angle_spline$x[angle_range]
    diff<-n_spline$y[2:length(n_spline$y)] - n_spline$y[1:(length(n_spline$y)-1)]
    diff_2nd<-diff[2:length(diff)]-diff[1:(length(diff)-1)]
    ########################
    ##run some logic to figure out which points to choose between
#     plot(n_spline)
#     plot(diff)
#     plot(diff_2nd)
    
    ###Logic begin
    #if(sum(diff_2nd>0)==length(diff_2nd)){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points'))}
    #if(!is.finite(infl)){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points'))}
   
    #infl<-which(diff_2nd[1:(length(diff_2nd)-1)]>0 & diff_2nd[2:length(diff_2nd)]<0 & (angle_spline$x>=92)[2:(length(angle_spline$x)-2)] )[1] +1
    #if(!is.finite(infl)){infl<-length(n_spline$x)}
    
    start<-(which(diff[1:(length(diff)-1)]<0 & diff[2:length(diff)]>0 & angle_spline$x[2:(length(n_spline$x)-1)]<96 & (2:length(diff))<length(diff)*.75) +2  )[1]
    
    
    if(!is.finite(start)){start<-1}
    if(diff[start+1]<(max(diff)*per_rate)){start<-(which(diff[2:length(diff)]>(max(diff)*per_rate) & (2:length(diff))>=start) +1)[1]}
  
    if(!is.finite(start)){print(paste0(names(here)[[pp]],'_(',pp,'): needs to be fit manually')); next}
    
    ##recalculate the inflection with the new start point established
    infl<-which(diff_2nd[1:(length(diff_2nd)-1)]>0 & diff_2nd[2:length(diff_2nd)]<0 & (angle_spline$x>=92)[2:(length(angle_spline$x)-2)] &  n_spline$x[2:(length(n_spline$x)-2)]>=n_spline$x[2:(length(n_spline$x)-2)][start] & (n_spline$x<(n_spline$x[start]+.75))[2:(length(n_spline$x)-2)])
    
    
    if(length(infl)<1||!is.finite(infl)){infl<-length(n_spline$x)}
    infl<-infl[which.max(diff[infl-1])]
    if(infl<length(n_spline$x)){infl<-infl+1}
    
    if(!is.finite(infl) & (sum(n_spline$x>(n_spline$x[start] +  max_min/60))==0 )){print(paste0(names(here)[[pp]],'_(',pp,'): does not have enough data points')); next}
    
    if(infl<start){print(paste0(names(here)[[pp]],'_(',pp,'): needs to be manually fit')); next}
    
    if(n_spline$y[infl]<tol){print(paste0(names(here)[[pp]],'_(',pp,'): inflection point was below 1 Z. Check night, then manually fit')); next}
    
    
    
    
    n_spline$x[infl]
    n_spline$x[start] +  max_min/60
    choice<-which.min(c(n_spline$x[infl], n_spline$x[start] +  max_min/60))
    infl_final<-min(c(n_spline$x[infl], n_spline$x[start] +  max_min/60),na.rm=T)
    infl_final<-which(n_spline$x==incomp(n_spline$x,infl_final))
    
    ###check that the calculation is not over 102 degrees
    tt<-n_spline$x[infl_final]-12
    tt2<-as.POSIXct(paste0(my.data$date[1],' ',floor(tt) ,':', floor((tt-floor(tt)) *60),':' ,round(((tt-floor(tt)) *60-floor((tt-floor(tt)) *60)) *60)), format='%Y-%m-%d %H:%M:%S',tz='UTC') + 12*60*60
    sunangle<-SZA(tt2,lat,long)
    if(sunangle>102){print(paste0(names(here)[[pp]],'_(',pp,'): is 60 minutes after sunset, please check that fit is correct')); next}
    
    ###
    plot(or_spline$y ~ or_spline$x)
    title(main=paste0(names(here)[[pp]],'_(',pp,')'))
      abline(v=n_spline$x[infl_final],col='green')
      abline(v=n_spline$x[start],col='black')
      
      points(my.data$ref ~ my.data$d_time, col='red')
      #legend('bottomright', paste0(signif(n_spline$x[inf],4)),col=c('black'))
      answerframe$strength[pp]<-n_spline$y[infl_final]-n_spline$y[start]
      answerframe$start[pp]<-n_spline$x[start]
      answerframe$inflection[pp]<-n_spline$x[infl]
      answerframe$method[pp]<-c('inflection','start')[choice]
      answerframe$sample[pp]<-n_spline$x[infl_final]
      ####calculate sunset piecewise
      approx_sun<-floor(suncalc(as.POSIXlt(paste0(answerframe$date[pp],' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC')$yday, lat, long, UTC=F)$sunset + 4) 
      tt1<-as.POSIXlt((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3)[which.min(abs(SZA((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3),lat,long)-90))],format='%Y-%m-%d %H:%M:%S')
      answerframe$sunset[pp]<-to_dhour(tt1)
      abline(v=to_dhour(tt1),col='red')
      ####
      #warn if its above 102
      
  }
  
  bad<-numeric()
  if(sum(is.na(answerframe$inflection))>0){
    bad<-paste0(answerframe$date[is.na(answerframe$inflection)],'_(',which(is.na(answerframe$inflection)),')')}
  bad<<-bad
  answerframe<<-answerframe
  ref_data<<-here
  #bad
  ####writes out a file called 'bad' which can be referred to later
  ###calculates sunangle

  ##returns plot window to default values
  par( mfrow = c( 1, 1 ) )
  par ( mar= c(5, 4, 4, 2) )
  warning(paste0('red = sunset, black = start of exodus, green = sampling time, red circle = actual reflectivity, black circle = interpolated reflectivity'),call.=F)
}

