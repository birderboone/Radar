#corrected=F;lowerr=10000;upperr=40000;nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv"; waterfilt=T; waterfilt_op=0; terminal=F; parallel=T; legacy=F; clutter=T ;canada=F
#basegrid should include : id, range, azimtuh, groundht, pblock, waterfilt, clutter

#' Calculates the mean reflectivity of each sweep of .netcdf radar files. 
#' 
#' Used primarily for fitting exodus curves.
#' @param radar the 3 letter radar code
#' @param corrected is the data range corrected
#' @param lowerr lowest considered range in meters
#' @param higherr highest considered range in meters
#' @param years unique vector of years to be calculated from the available netcdf sweeps
#' @param in_netcdf the file path of the netcdfs
#' @param out_netcdf the location to save the file ref file
#' @param basegrid_n the location of the basegrid
#' @param mask the id numbers taken from the basegrid that you'd like to consider in calculation of reflectivity
#' @param waterfilt do you want to consider water in the mean calculation
#' @param waterfilt_op if waterfilt==T then whether you want to only look over water (1) or only over land (1)
#' @param terminal Is the radar data terminal doppler
#' @param parallel would you like to run the function using parallel computing
#' @param canada Is the radar data from canadian radar
#' @param legacy are the files legacy (warning: not rigorously tested)
#' @param clutter do you wante to filter out clutter
#' @param width what ranges from the height of the radar do you want to consider
#' @param z do you want the values to be converted to units Z
exodusangle_netcdf<-function(radar,corrected=F,lowerr=10000,upperr=40000,nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv",years=all_years,in_netcdf,out_netcdf, basegrid_n,mask=base$id, waterfilt=T,waterfilt_op=0, terminal=F, parallel=T, legacy=F, clutter=T, width=50, z=T,canada=F){
  
  require(ncdf4)
  require(RAtmosphere)
  require(foreign)
  
  ######################################################################################################
  ##                          Fitting a Generalized Logistic Curve to Exodus                          ##
  ##years to subset
  ######################################################################################################
  ### Dynamic variables
  ######################################################################################################
  ptm<-proc.time()
  ######################################################################################################
  ##                                      Do work                                                     ##
  ######################################################################################################
  nexrad1<-read.csv(nexrad_n)
  base<-read.dbf(basegrid_n)
  
  colnames(base)<-tolower(colnames(base))
  #mask=base$id
  if(clutter==T) {base<<-subset(base,clutter==0 & pblock <=0.25 & range>lowerr & range<upperr,select=c('id','range','azimuth','clutter','waterfilt', 'pblock','groundht'))}
  if(clutter==F) {base<<-subset(base,pblock <=0.25 & range>lowerr & range<upperr,select=c('id','range','azimuth','clutter','waterfilt', 'pblock','groundht'))}
  if(waterfilt==T){base<-subset(base,waterfilt==waterfilt_op) }
  
  #if(!missing(mask)){base<-base[base$id%in%mask,]}
  base<-base[base$id%in%mask,]
  
  nexrad<-subset(nexrad1,SITE==radar)
  lat<-nexrad$LAT
  long<-nexrad$LONG
  base_ht<-nexrad$BASE_HT_M
  testlist<-list()
  all_years<-unique(substr(list.files(in_netcdf),1,4))
  
  #years=all_years
  years<-as.character(years)
  files<<-list.files(in_netcdf)[substr(list.files(in_netcdf),1,4)%in%years]
  
  if( parallel==T ){
    require(parallel)
    cl<-makeCluster(detectCores())
    clusterSetRNGStream(cl)
    junk<- clusterEvalQ(cl, list(require(ncdf4), require(radar)))
    #c(in_netcdf,corrected,radar,lowerr,upperr,base_ht)
    base_id<<-base[,c('id','range','azimuth','groundht')]
    corrected<<-corrected
    lowerr<<-lowerr
    upperr<<-upperr
    base_ht<<-base_ht
    what<-clusterExport(cl, c('in_netcdf', 'corrected','radar','lowerr','upperr', 'base_id','base_ht'))
    
    testlist<-parLapply(cl=cl, files, function(i) {
      
      #k<-which(files==i)
      #i<-files[1]
      test<-nc_open(paste0(in_netcdf,'/',i))   
      if(corrected==F){ref<-ncvar_get(test, names(test$var)[grepl('^Reflectivity',names(test$var))], verbose=F)}
      #if(corrected==F){ref<-get.var.ncdf(test, 'Reflectivity', verbose=F)}
      if(corrected==T){ref<-ncvar_get(test, 'BirdReflectivity', verbose=F)}
      ref<-as.data.frame(ref)
      #get.var.ncdf(test, 'GateWidth', verbose=F)
      ref[ref<(-99000)]<-NA
      #if(z==T) {ref<-as.data.frame(dbz2z(ref))} else{ref <- as.data.frame(ref)}
      ref<-as.data.frame(dbz2z(ref))
      ref[is.na(ref)]<-0
      net_out<-with(test,{
        azimuth=ncvar_get(test, "Azimuth")
        rangetofirstgate=ncatt_get(test, 0, "RangeToFirstGate")
        rangetofirstgate=rangetofirstgate$value
        gate=ncvar_get(test,"Gate")
        gatewidth=ncvar_get(test,"GateWidth")
        gatewidth=round(gatewidth[1],0)
        if((radar%in%c('DOX') & as.numeric(substr(i,1,4)) <=2011)|(as.numeric(substr(i,1,4)) <=2007) ) {  range=(gate-1)*gatewidth+rangetofirstgate +125 } else {range=(gate-1)*gatewidth+rangetofirstgate}
        if(terminal==T){range<-gate*gatewidth}
        range<-round(range)
        #if(radar%in%c('DOX') & terminal==F) {  azimuth<-mround(azimuth, 0.25) + .25 } else {azimuth<-mround(azimuth, 0.25) }
        if(radar%in%c('DOX')){  azimuth<-mround(azimuth, 0.25) + .25 }
        if(!(radar%in%c('DOX')) & terminal==F & legacy==F) {azimuth<-mround(azimuth, 0.25) }
        if(terminal==T){azimuth<-mround(azimuth, 0.25) - .5}
        
        if(legacy==T){azimuth<-mround(azimuth,.5) + .25}
        if(canada==T){azimuth<-mround(azimuth,.5) +.25}
        list(range=range,azimuth=azimuth)
        
      })
      
      all_azimuth<-unique(net_out$azimuth)
      all_range<-unique(net_out$range)
      row.names(ref)<-net_out$range
      colnames(ref)<-net_out$azimuth
      something<-data.frame(ref=unlist(ref),
                            range=rep(as.numeric(row.names(ref)),ncol(ref)),
                            azimuth=rep(as.numeric(colnames(ref)),each=nrow(ref)))
      
      
      something<-subset(something,range>lowerr & range<upperr)
      
      ##########################
      re_range<-function(data,old_range, new_range=seq(250,100000,250),r_bind=T,tol=1000){
        tt<-lapply(new_range, function(z){
          az_a<-incomp(unique(old_range),z)
          if(abs(az_a-z)<=tol){
            dd<-subset(data, old_range==az_a)
            dd$range<-z
            return(dd)}
        })
        if(r_bind==T){df<-do.call(rbind, tt)}
        if(r_bind==F){df<-as.data.frame(tt)}
        return(df)
      }
      ###########################
      if(canada==T){
        something<-re_range(something, old_range=something$range, new_range=unique(base$range),tol=1000)
        all_range<-unique(something$range)
      }
      base_id<-base_id[base_id$range%in%all_range & base_id$azimuth %in%all_azimuth,]
      
      mergeframe<-merge(base_id,something,by=c('range','azimuth'),all.x=T)
      mergeframe1<-subset(mergeframe,groundht<=(base_ht + width) & groundht>=(base_ht-width)) 
      #mergeframe1$ref[is.na(mergeframe1$ref)]<-0   ###fill in NAs with zeros
      hh<-nc_close(test)
      
      mean_z<-list(name=i,num=mean(mergeframe1$ref*mergeframe1$hectares,na.rm=T,trim=0.15))
      mean_z
      
      
    })
    stopCluster(cl)
    
  }
  
  if( parallel==F ){
    
    base_id<-base[,c('id','range','azimuth','groundht')]
    for(i in files){
      #i<-files[1]
      k<-which(files==i)
      
      test<-nc_open(paste0(in_netcdf,'/',i))   
      if(corrected==F){ref<-ncvar_get(test, names(test$var)[grepl('^Reflectivity',names(test$var))], verbose=F)}
      #if(corrected==F){ref<-get.var.ncdf(test, 'Reflectivity', verbose=F)}
      if(corrected==T){ref<-ncvar_get(test, 'BirdReflectivity', verbose=F)}
      ref<-as.data.frame(ref)
      #get.var.ncdf(test, 'GateWidth', verbose=F)
      ref[ref<(-99000)]<-NA
      #if(z==T) {ref<-as.data.frame(dbz2z(ref))} else{ref <- as.data.frame(ref)}
      ref<-as.data.frame(dbz2z(ref))
      ref[is.na(ref)]<-0
      net_out<-with(test,{
        azimuth=ncvar_get(test, "Azimuth")
        rangetofirstgate=ncatt_get(test, 0, "RangeToFirstGate")
        rangetofirstgate=rangetofirstgate$value
        gate=ncvar_get(test,"Gate")
        gatewidth=ncvar_get(test,"GateWidth")
        gatewidth=round(gatewidth[1],0)
        if((radar%in%c('DOX') & as.numeric(substr(i,1,4)) <=2011)|(as.numeric(substr(i,1,4)) <=2007) ) {  range=(gate-1)*gatewidth+rangetofirstgate +125 } else {range=(gate-1)*gatewidth+rangetofirstgate}
        if(terminal==T){range<-gate*gatewidth}
        range<-round(range)
        #if(radar%in%c('DOX') & terminal==F) {  azimuth<-mround(azimuth, 0.25) + .25 } else {azimuth<-mround(azimuth, 0.25) }
        if(radar%in%c('DOX')){  azimuth<-mround(azimuth, 0.25) + .25 }
        if(!(radar%in%c('DOX')) & terminal==F & legacy==F) {azimuth<-mround(azimuth, 0.25) }
        if(terminal==T){azimuth<-mround(azimuth, 0.25) - .5}

        if(legacy==T){azimuth<-mround(azimuth,.5) + .25}
        if(canada==T){azimuth<-mround(azimuth,.5) +.25}
        list(range=range,azimuth=azimuth)
        
      })
      
      all_azimuth<-unique(net_out$azimuth)
      all_range<-unique(net_out$range)
      row.names(ref)<-net_out$range
      colnames(ref)<-net_out$azimuth
      something<-data.frame(ref=unlist(ref),
                            range=rep(as.numeric(row.names(ref)),ncol(ref)),
                            azimuth=rep(as.numeric(colnames(ref)),each=nrow(ref)))
      
      
      something<-subset(something,range>lowerr & range<upperr)
      
      ##########################
      re_range<-function(data,old_range, new_range=seq(250,100000,250),r_bind=T,tol=1000){
        tt<-lapply(new_range, function(z){
          az_a<-incomp(unique(old_range),z)
          if(abs(az_a-z)<=tol){
          dd<-subset(data, old_range==az_a)
          dd$range<-z
          return(dd)}
        })
        if(r_bind==T){df<-do.call(rbind, tt)}
        if(r_bind==F){df<-as.data.frame(tt)}
        return(df)
      }
      ###########################
      if(canada==T){
        something<-re_range(something, old_range=something$range, new_range=unique(base$range),tol=1000)
        all_range<-unique(something$range)
      }
      base_id<-base_id[base_id$range%in%all_range & base_id$azimuth %in%all_azimuth,]
      
      mergeframe<-merge(base_id,something,by=c('range','azimuth'),all.x=T)
      mergeframe1<-subset(mergeframe,groundht<=(base_ht + width) & groundht>=(base_ht-width)) 
      #mergeframe1$ref[is.na(mergeframe1$ref)]<-0   ###fill in NAs with zeros
      hh<-nc_close(test)
      
      
      mean_z<-list(name=i,num=mean(mergeframe1$ref,na.rm=T,trim=0.15))
      testlist[[i]]<-mean_z
      
      
      
      ###################################
      total <- length(unique(files))
      pb <- txtProgressBar(min = 0, max = total, style = 3)
      setTxtProgressBar(pb, k)
      ###################################
      
    }
    stopCluster(cl)
    
  }
  
  
  
  ###################
  
  test_names<-lapply(testlist, function (x) x$name)
  testlist<-lapply(testlist, function (x) x$num)
  names(testlist)<-test_names
  
  netcdf<<-with(testlist, {
    times = as.POSIXlt(substr(names(testlist),1,15),format='%Y%m%d-%H%M%S',tz='EST');
    ref = unlist(testlist);
    sunangle = SZA(times, lat, long);
    date = substr(times-14400, 1, 10);
    data.frame(times, ref, sunangle, date)
  })
  if(z==F) {netcdf$ref<-z2dbz(netcdf$ref)}
  write.csv(netcdf,out_netcdf,row.names=F)
  proc.time() - ptm
}
