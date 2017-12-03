##TROUBLESHOOTING (should remain commented out if not troubleshooting)
#out=paste0(ext1,'/nb_sandy'); corrected=F; sun=F; sum_angle=1.5;vprtop=1500;col_time=T
##
#' Creates a summary file for multiple Interpolated Reflectivity files that filters out detection distance
#' 
#' This function will insert NAs for correction factors <0.5, correct data using the vpr, and insert all nights into one large summary file thats merged with the basegrid. 
#' @param radar The 3 letter radar code
#' @param folder The project name that will be tacked onto the name of the files
#' @param basegrid_n The location of the basegrid
#' @param out Folder to write sumamry file
#' @param refdir Location of the InterpolatedReflectivity files
#' @param vpdir Location of the BirdVPRfiles
#' @param zoption What units would you like to convert reflectivity too (1: Z, 2: cm2/km3, 3: cm2/hectare)
#' @param header Do the vprs contain a header?
#' @param legacy Is the input from legacy data?
#' @param vprbottom What is the height of the bottom of the vpr?
#' @param vprtop What is the height of the vpr?
#' @param corrected Are the files corrected, if F it will correct using the vpr
#' @param start Override function indicating the file number you want to begin running
#' @param end Override function indicating the file number you want to end on
#' @param sun Would you like to filter out the location of a possible sun flare?
#' @param sum_angle What angle from the Interpolated Reflectivity files do you want to create the summary for?
#' @param col_time Do you want the column names to include the time (in decimal hour)
summary_exodus<-function(radar, folder, basegrid_n, out, refdir, vprdir, zoption=3, header=T, legacy=F, vprbottom=10, vprtop=1500, nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv",corrected=T, start=1, end=length(files),sun=F, sum_angle=0.5,col_time=F,prop=.1){
  #header=T; legacy=F; seperate1=T; vprbottom=10; vprtop=1500; nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv"
  require(foreign)
  require(radar)
  require(RAtmosphere)
  print(paste0('currently summarizing ',radar,'_' ,folder))
  ptm <- proc.time()
  
  ####get nexrad site parameters
  nexrad_get(nexrad_n=nexrad_n, radar=radar)
  ####read in basegrid
  
  basegridb<-read.dbf(basegrid_n)
  colnames(basegridb)<-tolower(colnames(basegridb))
  ra<-basegridb[,c('id','range','azimuth')]
  finalframe.all<-basegridb
  testgrid<-basegridb
  
  #############################
  #recalculate beam heights
  testgrid<-testgrid[testgrid$clutter==0,]
  testgrid<-cbind(testgrid, beamht_std(range=testgrid$range, groundht=testgrid$groundht,elev=sum_angle,radar=radar,nexrad_n=nexrad_n))
  
  ###list reflectivity files
  setwd(refdir)
  files<-list.files()
  vprlist<-list.files(vprdir)
  ##################################################
  ### quick check to make sure every reflectivity file has a vpr file
  check1<-(substr(as.POSIXlt(substr(files,26,40),format='%Y%m%d-%H%M%S',tz='UTC') - 60*60 *6,1,10) %in% substr(as.POSIXlt(substr(vprlist,14,28),format='%Y%m%d-%H%M%S',tz='UTC') - 60*60 *6,1,10))
  if( sum(!check1)>0) {
    stop(paste0(files[!check1], ' does not have an associated vpr'))}
  
  tryCatch(setwd(out), error=function(x) print("can't change to output directory") )
  ### start reading through each file
  finalist<-list()
  good_ob<-NA
  good_ob<-ls()
  for(l in files[start:end]){
    #l<-files[1]
    ###we'll read in the vpr first and leave it if need be
    setwd(vprdir)
    vprfile<-vprlist[which.min(abs(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S',tz='UTC')-as.POSIXlt(substr(vprlist,14,28),format='%Y%m%d-%H%M%S',tz='UTC')))]
    if(abs(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S',tz='UTC')-as.POSIXlt(substr(vprfile,14,28),format='%Y%m%d-%H%M%S',tz='UTC'))> 60*60*6){stop(paste0(l,' does not have an associated vpr within 6 hours of it'))}
    vpr3<-read.csv(vprfile,header=header)
    
    colnames(vpr3)<-c('hght','zh1')   
    vpr3$hght<-vpr3$hght*1000
    
    ###this fixes something, I forget what though
    if(max(vpr3$hght)<vprtop){
      vpr3<-merge(vpr3,data.frame(hght=seq(vprbottom,vprtop,10)),by='hght',all.y=T)
      vpr3$zh1[1:min(which(!(is.na(vpr3$zh1))))]<-vpr3$zh1[min(which(!(is.na(vpr3$zh1))))]
      vpr3$zh1[(max(which(!(is.na(vpr3$zh1))))+1):(vprtop/10)]<-0
    }
    #your vpr is now ready for whatever you throw at it
    #now to deal with the reflectivity
    ##############
    setwd(refdir)
    
    file<-read.csv(l)
    ##this is the night we're refering too
    time<-as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S',tz='UTC')
    date1<-substr(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S',tz='UTC') - 60*60 *6,1,10)
    ##this is the actual night date (past midnight = next day)
    date2<-substr(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S',tz='UTC'),1,10)
    date_real<-paste0(substr(date1,1,4),substr(date1,6,7),substr(date1,9,10))
    date_file<-paste0(substr(date2,1,4),substr(date2,6,7),substr(date2,9,10))
    
    #colnames(file)<-c('azimuth','range','z_raw','elev') ###only for GL
    
    #####################################################################################
    #   re-calculate beam ht angles
    #####################################################################################
    
    
    ###subset to the 0.5 elevation
    refframe<-subset(file,signif(elev,2)==sum_angle)
    #######need to correct
    if(corrected==F){refframe$correction<-NA ; refframe$z_corr <- NA}
    ###a special 'if', depending on the radar and year, this vector of radars can & will grow
    if(as.numeric(substr(l,26,29))<=2011 & radar%in%c('DOX','MOB','LTX')){
      refframe$range <- refframe$range*1000+125 } else {
        refframe$range <- refframe$range*1000} 
    
    refframe$range <- round(refframe$range)
    refframe$azimuth <- refframe$azimuth+0.25
    rangefloor <- min(refframe$range,na.rm=T)
    refframe <- merge(testgrid,refframe,by=c('range','azimuth'),all.x=T)
    ####### sun filter ########
    
    ##subset grid starting from the closest measure range and replace NA with 0's
    #this is purely a check to make sure the reflecitivity and basegrid match
    refframe<-refframe[refframe$range>=rangefloor,]
    #######################################################
    refframe$z_raw[is.na(refframe$z_raw)]<-0
    ###group by commong pblock, groundht, and range
    
    common_par<-aggregate(refframe[,c('range','pblock','groundht','correction','id')],by=list(refframe$range,refframe$pblock,refframe$groundht),mean,na.rm=T)
    
    colnames(common_par)[colnames(common_par)=='correction']<-'za2'
    bad<-merge(refframe[,c('range','pblock','groundht','correction')],common_par,by=c('range','pblock','groundht'),all.x=T)
    
    ###we order them because we know they're the same length, but we want orders the same so we can just replace values
    bad<-bad[order(bad$id),]
    refframe<-refframe[order(refframe$id),]
    ##replace missing correction values with common correction values
    refframe$correction[is.na(bad$correction)]<-bad$za2[is.na(bad$correction)]
    
    ###we're now going to take the remaining NAs and recalulate their correction factors
    recalframe<-refframe[is.na(refframe$correction),]
    if(nrow(recalframe)>0){ bad<-recal_corr(data=recalframe, vpr=vpr3, elev=sum_angle) 
    
    refframe<-refframe[order(refframe$id),]
    ##merge with the original grid and fill in missing values with za2
    bad<-merge(refframe[,c('range','groundht','pblock','correction','id','z_raw')],bad,by=c('range','groundht','pblock'),all.x=T)
    bad<-bad[order(bad$id),]
    refframe$correction[is.na(bad$correction)]<-bad$za[is.na(bad$correction)]
    refframe$propdist<-bad$propdist
    }
    
    if(corrected==F){refframe$z_corr<-refframe$z_raw/refframe$correction}
    ##the rest are 0s
    refframe$z_corr[is.na(refframe$z_corr)]<-0
    
    ##fill in what should really be NA's
    refframe$z_corr[refframe$clutter==1]<-NA ##this is for z(i) 
    refframe$z_corr[is.na(refframe$correction)]<-NA
    refframe$z_corr[refframe$correction<.05]<-NA
    refframe$z_corr[refframe$propdist<prop]<-NA
    refframe$z_corr[refframe$z_corr>3160]<-NA
    if(SZA(time, Lat=lat, Lon=long)> 87 & SZA(time, Lat=lat, Lon=long)<93 & sun==T){
      sun_a<-sunpos(sunvector(JD(time), lat, long, 0))[1]
      refframe$z_corr[refframe$azimuth < (sun_a +1) & refframe$azimuth> (sun_a - 1)] <- NA
    }
    ###save into our list
    
    if(col_time==F){colnames(refframe)[colnames(refframe)=='z_corr']<-paste0('z',substr(date_real,3,8))}
    if(col_time==T){colnames(refframe)[colnames(refframe)=='z_corr']<-paste0('z',date2,'_',signif(to_dhour(time),4))}
    if(col_time==F){finalist[[l]]<-merge(ra,refframe,by='id',all.x=T)[,paste0('z',substr(date_real,3,8)),drop=F]}
    if(col_time==T){finalist[[l]]<-merge(ra,refframe,by='id',all.x=T)[,paste0('z',date2,'_',signif(to_dhour(time),4)),drop=F]}
    ###### quick check that theres not duplicate rows
    if(nrow(finalist[[l]])!=nrow(ra)){stop(paste0('duplicate range and azimuth entries in file: ', l))}
        ###############################clock#############################################
      total <- length(files)
      pb <- txtProgressBar(min = 0, max = total, style = 3)
      setTxtProgressBar(pb, which(files==l))
      
      #################################################################################
      ##########################3
      # clean house
      rm(list=ls()[!ls()%in%good_ob])
    } ##end l loop
    finalframe.all<-cbind(finalframe.all,as.data.frame(finalist))
    
    if (zoption==1) {
      finalframe.all<-finalframe.all
    } else if (zoption==2) {
      finalframe.all[,grep("^z", colnames(finalframe.all))]<-finalframe.all[,grep("^z", colnames(finalframe.all))]*(0.93*(10**3)*(pi**5)/(10.7**4))
    } else if (zoption==3) {
      finalframe.all[,grep("^z", colnames(finalframe.all))]<-(finalframe.all[,grep("^z", colnames(finalframe.all))]*(0.93*(10**3)*(pi**5)/(10.7**4))*(vprtop/1000))/100
    }
    
    #########################################WRITE!##################################################
    setwd(out)
    
    write.csv(finalframe.all,paste0('K',radar,'_summary_',folder,'.csv'),na='',row.names=FALSE)
    
    # Stop the progressbar
    #close(pb)
    # stop the clock
    proc.time() - ptm
  }