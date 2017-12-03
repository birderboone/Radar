exodus_fit_spline<-function(nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv" ,out_netcdf, radar){
  require(RAtmosphere)
  require(nlme)
  nexrad_get(radar=radar)

  final<-read.csv(out_netcdf)
  
  times<-as.POSIXlt(final$times,format='%Y-%m-%d %H:%M')
  final$d_time<-to_dhour(times)
  final$d_time[final$d_time<12]<-final$d_time[final$d_time<12] + 24
  final$sunangle<-SZA(final$times, Lat=lat, Lon=long)
  final$ref<-final$ref+abs(min(final$ref))
  here<-by(final,list(final$date),data.frame)
  
  answerframe<-as.data.frame(matrix(ncol=5, nrow=length(here)))
    colnames(answerframe)<-c('start','inflection','sunset','strength','date')
 answerframe$date<-names(here)
 error<-list()
  #final<-final[order(final$times),]
  ################################################
  ###TAKE A BREAK SON
  ################################################
  ##changes plot window
  par( mfrow = c( 4, 4 ) )
  par(mar=c(1.75,1,1.75,1))
  ################################################
  for(pp in 1:dim(here)){ 
    #for(pp in 106:110){ 
    #pp<-6
    my.data<-here[[pp]]
    my.data<-my.data[order(my.data$d_time),]
    nspline<-round((max(my.data$d_time) - min(my.data$d_time)) * 30)
    groupd<-groupedData(ref~d_time|date, data=my.data)
    groupd<-groupd[order(groupd$sunangle),]
    groupd<-groupd[groupd$sunangle>87,]
    end<-which.max(groupd$ref) + 3
    if(nrow(groupd)<end){end<-nrow(groupd)}
    groupd<-groupd[1:end,]
    
    if(dim(groupd)[1]<5) {print(paste0(names(here)[[pp]],'_(',pp,') has too few observations')); next}
    
    n_spline<-spline(groupd$d_time, groupd$ref,n=nspline)
    or_spline<-n_spline
    angle_range<-which(spline(groupd$sunangle, groupd$ref,n=nspline)$x<=100 & spline(groupd$sunangle, groupd$ref,n=nspline)$x>=90) 
    if(length(angle_range)<2){print(paste0(names(here)[[pp]],'_(',pp,'): not enough angles below 100')); next}
    n_spline$y<-n_spline$y[angle_range]
    n_spline$x<-n_spline$x[angle_range]
    diff<-n_spline$y[2:length(n_spline$y)] - n_spline$y[1:(length(n_spline$y)-1)]
    ########################
    ##run some logic to figure out which points to choose between
    #plot(n_spline)
    #plot(diff)
    diff_2nd<-diff[2:length(diff)]-diff[1:(length(diff)-1)]
    #plot(diff_2nd)
    minima<-sapply((2:(length(diff_2nd)-1)), function (x){
      if(diff[x+1]>0 & diff[x-1]<0 & diff[x]>0){x}else{NA}})
    minima<-minima[!is.na(minima)]
    if(is.logical(minima)){minima<-1}
    minima<-minima[(minima-(which.min(n_spline$y)-1))>0]
    minima<-min(minima)
    if(!is.finite(minima)){minima<-1}
    
    maxima<-sapply((2:(length(diff_2nd)-1)), function (x){
      if(diff_2nd[x+1]<0 & diff_2nd[x-1]>0 & diff_2nd[x]<0){x}else{NA}})
    maxima<-maxima[!is.na(maxima)]
    maxima<-maxima[(maxima-minima)>0]
    maxima<-maxima[spline(groupd$sunangle, groupd$ref,n=nspline)$x[maxima]>92]
    maxima<-maxima[(n_spline$y[maxima]-n_spline$y[minima])>.05]
    maxima<-maxima[which.min(maxima-minima)]
    if(is.logical(maxima)){print(paste0(names(here)[[pp]],'_(',pp,'): fit was above 100 degrees')); next}
    if(length(maxima)==0){print(paste0(names(here)[[pp]],'_(',pp,'): fit was below 92 degrees')); next}
    ####
    inf<-which(diff==max(diff[(minima):(maxima+1)]))
    if(inf%in%c(0,1,2,(length(n_spline$y)-2):length(n_spline$y))){print(paste0(names(here)[[pp]],'_(',pp,'): fit was above 100 degrees')); next}
    #start_v<-min(n_spline$y) +(n_spline$y[inf] - min(n_spline$y))*.
    #start<-max(which(diff[1:inf]<0)) +2
    tt<-min(which(diff[inf:1]<0))
    if(is.infinite(tt)){tt<-(inf-1)}
    start<-inf-tt+1
    
    start_v<-(n_spline$y[inf] - n_spline$y[start]) *0.1 + n_spline$y[start]
   
    start_v1<-which(n_spline$x==n_spline$x[start:inf][which.min(abs( n_spline$y[start:inf]-start_v))])
    #if((n_spline$x[inf]-n_spline$x[start_v1]) > .33){print(paste0(names(here)[[pp]],'_(',pp,'): start and inflection times too far off')); next}
    if((n_spline$y[inf]-n_spline$y[start]) > 100){print(paste0(names(here)[[pp]],'_(',pp,'): strength is too high, check fit')); next}
  # start_v<-n_spline$y[which.max(which(diff[1:inf]<0))] *1.05
  # which.max(which(diff[1:inf]<0)):inf
   # start<-which.min(abs(n_spline$y -start_v))
    plot(or_spline$y ~ or_spline$x)
    title(main=paste0(names(here)[[pp]],'_(',pp,')'))
      abline(v=n_spline$x[inf],col='green')
      abline(v=n_spline$x[start_v1],col='black')
      
      points(my.data$ref ~ my.data$d_time, col='red')
      #legend('bottomright', paste0(signif(n_spline$x[inf],4)),col=c('black'))
      answerframe$strength[pp]<-n_spline$y[inf]-n_spline$y[start]
      answerframe$start[pp]<-n_spline$x[start]
      answerframe$inflection[pp]<-n_spline$x[inf]

      ####calculate sunset piecewise
      approx_sun<-floor(suncalc(as.POSIXlt(paste0(answerframe$date[pp],' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC')$yday, lat, long, UTC=F)$sunset + 4) 
      tt1<-as.POSIXlt((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3)[which.min(abs(SZA((as.POSIXlt(paste0(answerframe$date[pp],' ',approx_sun,':00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:3600*3),lat,long)-90))],format='%Y-%m-%d %H:%M:%S')
      answerframe$sunset[pp]<-to_dhour(tt1)
      abline(v=to_dhour(tt1),col='red')
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
}

