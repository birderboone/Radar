###
# vprdir<-"C:/Users/Matt/Downloads/package/DOX/vad/vpr"
# veldir   = "C:/Users/Matt/Downloads/package/DOX/vad/velocity/"
# basegrid_n = 'C:/Users/Matt/Downloads/package/DOX/KDOX_100km_super_grid_UTM.dbf'
# nexrad_n = 'C:/Users/Matt/Downloads/package/nexrad_site_list_with_utm.csv'
# narr_dir <-  "C:/Users/Matt/Downloads/package/DOX/vad/narr"
# out_dir <- "C:/Users/Matt/Downloads/package/DOX/vad/airspeeds"
# radar = 'DOX'
# 
# VAD(radar=radar, vprdir=vprdir, veldir=veldir,basegrid_n=basegrid_n, nexrad_n=nexrad_n, narr_dir=narr_dir, out_dir=out_dir)
##########

VAD<-function(radar,vprdir, veldir, basegrid_n,...,narr_dir,out_dir,nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv", elev=2.5,skip=T,min_min=30){
#elev   =  2.5


###
require(foreign)
require(NADA)
#require(radar)
  nexrad_get(radar=radar, nexrad_n=nexrad_n)
  
ptm <- proc.time()

files <- list.files(veldir,pattern="\\.csv$")
vpr_files<- list.files(vprdir,pattern="\\.csv$")
vpr_date<-substr(vpr_files,14,21)
vpr_times<-as.POSIXlt(substr(vpr_files,14,28),format='%Y%m%d-%H%M%S',tz='UTC')
narr_files<-list.files(narr_dir, pattern='\\.csv$')
narr_times<-as.POSIXlt(paste0(substr(narr_files, 6,16),'0000'), format='%Y%m%d_%H%M%S',tz='UTC')
narr_files<-narr_files[order(narr_times)]
narr_times<-narr_times[order(narr_times)]
  
#read in basegrid
basegrid<-read.dbf(basegrid_n)
colnames(basegrid)<-tolower(colnames(basegrid))

mean.all<-list()
for(i in files){
  #i<-files[1]
  #read in vpr or skip
  vel_time<-as.POSIXlt(substr(i,5,19),format='%Y%m%d_%H%M%S',tz='UTC')
  #k<-vpr_files[(vpr_times-vel_time)==min(vpr_times-vel_time)& min(abs(vpr_times-vel_time))<(Sys.time() -Sys.time() + 60*min_min)] #wrong
  k<-vpr_files[abs(vpr_times-vel_time)< time_diff(min_min,'min')][1]
  poss.error<-tryCatch(vpr<-read.csv(paste0(vprdir,'/',k)), error=function(e)e)
  if(inherits(poss.error,'error')){print(paste0('file did not have an associated vpr, skipping: ',i));ifelse(skip==T,next,stop('stopping process',call.=F))}
  
  ####
  
  frame1<-read.csv(paste0(veldir,'/',i))
  colnames(frame1)<-c('range', 'azimuth', 'velocity')
  ####
 
  #fix azimuth
  
  frame1$azimuth<-mround(frame1$azimuth, .25)
  ###
  if(radar=='DOX'){frame1$azimuth<-frame1$azimuth+0.25}
  frame1<-subset(frame1, velocity!="NaN" & velocity<1000)
  
  meangrd <- weighted.mean(basegrid$groundht, basegrid$hectares)
  
  frame1<-merge(frame1, basegrid, by=c('range','azimuth'))
  frame1<-subset(frame1, clutter==0)  
  
  #make list
  range.list<-by(subset(frame1,range<70000), subset(frame1,range<70000)$range, list)
  
  #### apply Browning and Wexler (1968) equation (see also Gao et al. 2004)
  #### to determine horizontal velocities of targets from radial velocity
  
  ## where w0 is positive down, u0 (zonal) is positive east, and 
  ## v0 (meridional) is positive north
  
  here<-lapply(range.list, function(x){
    #x<-range.list[[1]]
    #for(i in 1:272){
      
      #x<-range.list[[i]]
      if (nrow(x) < 10) {}else{
    mod       <-nls(velocity~u0*cos( elev*deg2rad)*sin(azimuth*deg2rad ) + 
                      v0*cos( elev*deg2rad)*cos(azimuth*deg2rad) + w0*sin(elev*deg2rad),
                    start=(list(w0=0,u0=0,v0=0)), data=x,control= c(warnOnly = TRUE))
    r2adj     <-(1-(deviance(mod)/sum((x$velocity-mean(x$velocity))^2)))
    w0        <-summary(mod)$coefficients[1, 1]
    u0        <-summary(mod)$coefficients[2, 1]
    v0        <-summary(mod)$coefficients[3, 1]
    N         <-nrow(x)
    azimuth.width <- N
    c(w0,u0,v0,r2adj, N,azimuth.width)}
  })
  vel.data<-as.data.frame(do.call(rbind,here))
  colnames(vel.data)<-c('w0','u0','v0','r2adj','N','azimuth_width')
  vel.data$range<-as.numeric(row.names(vel.data))
  ## calculate "non-zero Fourier coefficients" (see Browning and 
  ## Wexler 1968) to predict horizontal target speed and direction 
 vel.data<- within(vel.data, {
    a1        <- -( u0*cos( elev*deg2rad ))
    b1        <- -( v0*cos( elev*deg2rad ))
    speed     <-  (sqrt( a1^2 + b1^2 ))/cos( elev*deg2rad)
    direction <- ifelse( a1 >= 0 , rad2deg*((pi/2 ) - atan( b1/a1 )), rad2deg*( ( ( 3*pi )/2) - atan( b1/a1)))
  })
  
 ## calculate beam heights ASL 
 vel.data<-cbind(vel.data,beamht_std(range=vel.data$range, elev=elev,radar=radar,nexrad_n=nexrad_n))
 
  ## add date and time
  date      <- substr(i,5,19)

  ###########
  # read in vpr
  
  

  time<-as.POSIXlt(substr(i,5,19), format='%Y%m%d_%H%M%S',tz='UTC')
  colnames(vpr) <- c('height', 'zh')
  
  vpr<-within(vpr, {
    minbinht = height*1000
    centerbinht = height*1000 + 5
    maxbinht = height*1000 + 10
  })
### read in NARR data
  ## NARR data geopotential height is in meters above sea leavel and winds are in m/s
     
  time1<-narr_times[abs(narr_times - time) <= time_diff(3,'hour')][1]
  time2<-narr_times[abs(narr_times - time) <= time_diff(3,'hour')][2]
  if(is.na(time2)){stop(paste0(date, ' does not have a before and after narr file'))}
  
  file1<-narr_files[abs(narr_times - time) < time_diff(3,'hour')][1]
  file2<-narr_files[abs(narr_times - time) < time_diff(3,'hour')][2]
  
  ###interpolate winds
  radio.list<-lapply(c(file1,file2), function(radio) {
    radio <- read.csv(paste0(narr_dir,'/',file1))
    radio$centerbinht <- (floor((radio$height.values - meangrd)/10)*10)+5
    radio <- subset(radio, centerbinht<2000)
    radio.u<-as.data.frame(approx(radio$centerbinht, radio$uwind.values, vpr$centerbinht, rule=2))
    colnames(radio.u)<-c('centerbinht', 'uwind.values')
    radio.v<-as.data.frame(approx(radio$centerbinht, radio$vwind.values, vpr$centerbinht, rule=2))
    colnames(radio.v)<-c('centerbinht', 'vwind.values')
    radio<-merge(radio.u, radio.v)
    radio
  })
  names(radio.list) <- c(time1, time2)
  
  radio.before<-radio.list[[1]]
  radio.after<-radio.list[[2]]
  
  radio.all<-merge(radio.before, radio.after, by="centerbinht")
  ##calculate weights
  time1_d<-to_dhour(time1, mid=T)
  time2_d<-to_dhour(time2, mid=T)
  time_d<-to_dhour(time,mid=T)
  weight1<-(time2_d-time_d)/3
  weight2<-(time_d-time1_d)/3
  
  uwind <- weight1*radio.all$uwind.values.x + weight2*radio.all$uwind.values.y
  vwind <- weight1*radio.all$vwind.values.x + weight2*radio.all$vwind.values.y
  radio <- data.frame(centerbinht=radio.all$centerbinht, uwind.values=uwind, vwind.values=vwind)
  radio$sondeSpeed <- (sqrt( radio$uwind.values^2 + radio$vwind.values^2 ))/cos( elev*deg2rad)
  radio$sondeDir <- 180/pi * atan2( radio$uwind.values, radio$vwind.values )
  radio$sondeDir <- ifelse (radio$sondeDir <= 180, (radio$sondeDir+180), (radio$sondeDir-180)) 
  
  radio<-merge(vpr, radio, by="centerbinht", all.y=T)
  
  vel.data$centerbinht<-(floor(vel.data$mbeam/10)*10)+5
  minht<-min(vel.data$centerbinht)

  if (nrow(radio) < 1) { next  } else {
    
    
    #Make airspeeds table##########################################################################################################
    
    radio<-merge(radio,vel.data, by="centerbinht")
    
    radio['groundspeed'] = radio$speed
    #radio$direction <- ifelse ( radio$a1 >= 0, (rad2deg*( ( PI/2 ) - atan( radio$b1/radio$a1 ) )), (rad2deg*( ( ( 3*PI )/2 ) - atan( radio$b1/radio$a1 ) )))
    radio['windx']   <- (radio$sondeSpeed*cos(deg2rad*(radio$sondeDir+180)))
    radio['windy']   <- (radio$sondeSpeed*sin(deg2rad*(radio$sondeDir+180)))
    radio['targetx'] <- (radio$groundspeed*cos(deg2rad*(radio$direction+180)))-(radio$sondeSpeed*cos(deg2rad*(radio$sondeDir+180)))
    radio['targety'] <- (radio$groundspeed*sin(deg2rad*(radio$direction+180)))-(radio$sondeSpeed*sin(deg2rad*(radio$sondeDir+180)))
    radio['windspeed'] = radio$sondeSpeed
    radio$groundheading<- ifelse ( radio$direction <= 180, (radio$direction+180), (radio$direction-180))
    radio$windheading<- ifelse (radio$sondeDir <= 180, (radio$sondeDir+180), (radio$sondeDir-180)) 
    radio$targetspeed<- sqrt(radio$targetx^2+radio$targety^2)
    radio$targetheading <- ifelse (radio$targety<0 & radio$targetx>0, (360+rad2deg*(atan(radio$targety/radio$targetx))),
                                   ifelse (radio$targetx<0, (rad2deg*(atan(radio$targety/radio$targetx))+180), (rad2deg*(atan(radio$targety/radio$targetx)))))
    
    # radio['groundx'] = (radio$groundspeed*cos(deg2rad*(radio$groundheading)))
    # radio['groundy'] = (radio$groundspeed*sin(deg2rad*(radio$groundheading)))
    radio['gheadx'] = cos(deg2rad*(radio$groundheading))
    radio['gheady'] = sin(deg2rad*(radio$groundheading))
    
    filenameair  <-paste0(out_dir,'/',"K",radar,"_", date, "_AIRSPEEDS","_", elev,".csv", sep="")
    
    write.csv(radio, filenameair, row.names=F) 
    
    ###Create summary file
    totalzh<-sum(radio$zh, na.rm=T)
    radio<-subset(radio, radio$r2adj >= 0.5 & radio$N >= 45 & radio$maxbinht >= minht & radio$zh>0)
    propzh<- sum(radio$zh)/(totalzh)  
    
    r2adj=weighted.mean(radio$r2adj,radio$zh) 
    windspeed = weighted.mean(radio$windspeed, radio$zh)
    targetspeed = weighted.mean(radio$targetspeed, radio$zh)
    groundspeed = weighted.mean(radio$groundspeed, radio$zh)
    gheadx = weighted.mean(radio$gheadx, radio$zh)
    gheady = weighted.mean(radio$gheady, radio$zh)
    targetx = weighted.mean(radio$targetx, radio$zh)
    targety = weighted.mean(radio$targety, radio$zh)
    windx = weighted.mean(radio$windx, radio$zh)
    windy = weighted.mean(radio$windy, radio$zh) 
    windheading<- ifelse (windy<0 & windx>0, (rad2deg*(atan(windy/windx))+360),
                          ifelse (windx<0, (rad2deg*(atan(windy/windx))+180), (rad2deg*(atan(windy/windx)))))
    
    groundheading<- ifelse (gheady<0 & gheadx>0, (rad2deg*(atan(gheady/gheadx))+360),
                            ifelse (gheadx<0, (rad2deg*(atan(gheady/gheadx))+180), (rad2deg*(atan(gheady/gheadx)))))
    targetheading <- ifelse (targety<0 & targetx>0, (360+rad2deg*(atan(targety/targetx))),
                             ifelse (targetx<0, (rad2deg*(atan(targety/targetx))+180), (rad2deg*(atan(targety/targetx)))))
    
    mean.all[[date]] <- data.frame(date=substr(unique(date),1,8),time=paste0(substr(unique(date),10,11),':', substr(unique(date),12,13),':' ,substr(unique(date),14,15)),targetspeed,targetheading, windspeed,windheading,groundspeed,groundheading,r2adj,propzh)
     
  }
  clockz(files,i)
}

final<-do.call(rbind, mean.all)
meanair  <-paste0("K",radar,"_", "meanairspeeds_narr",".csv", sep="")
write.csv(final, paste0(out_dir,'/',meanair), row.names=F)


# Stop the clock
proc.time() - ptm
}