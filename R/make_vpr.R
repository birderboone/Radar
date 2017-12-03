# # for debugging purposes 
# # library(radar)
# # radar
# # basegrid_n
# # vprdir
# # refdir
# # radio_fold
# # radiosonde
# nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv"
# mask
# z_min=0
# z_max=3165
# start=1
# end=length(files)
# minvprrange=5000
# maxvprrange=20000
# vprbottom=10
# vprtop=1500
# overwrite=T
# sun=T
# elev_max=5
# ###############################################################
# make_vpr(radar=radar, basegrid_n=basegrid_n, nexrad_n=nexrad_n, vprdir=vprdir, refdir=refdir, radio_fold=radio_fold, radiosonde=radiosonde, z_min=z_min, z_max, minvprrange=5000, maxvprrange=50000, overwrite=T)
##########################################################################################
#' Makes VPR's from interpolated reflectivity files
#' 
#'The majority of these arguments are special case scenarios and have default values. The only ones required are: radar, basegridn_n, vprdir, refdir, radio_fold, and radiosonde
#' @param radar The 3 letter radar code
#' @param basegrid_n The location of the basegrid
#' @param vprdir Location of the vpr files
#' @param refdir Location of the reflectivity files
#' @param radio_fold Location of the radiosondes, if applicable 
#' @param radisonde Does the radar have radiosonde
#' @param nexrad_n Location of the nexrad site table. Default is the location on the AP's server
#' @param mask A vector of the ID of pulse volumes you want considered in the VPR calculation
#' @param z_min The minimum reflectivity considered for vpr calculation. Filtered out based on the 0.5 angle.
#' @param z_max The maximum reflectivity considered for vpr calculation.
#' @param start An override value, describing what number of interpolated reflectivity files to start from
#' @param end An override value, described what number of interpolated reflectivity files to end from
#' @param minvprrange The minimum range from the radar to consider for vpr calculation (in meters)
#' @param maxvprrange The maximum range from the radar to consider for vpr calculation (in meters)
#' @param vprbottom The lowest height bin to calculate vpr values
#' @param vprtop The heighst height bin to calculate vpr values
#' @param overwrite Whether you'd like to overwrite already made VPRS or skip them
#' @param sun Filter out approximate location of sunflares in vpr calculation, only does so for angles 87 - 93
#' @param elev_min The lowest beam elevation to consider for vpr calculation
#' @param elev_max The heighest beam elevation to consider. It is safer to put a value higher than the heighest to be considered

make_vpr<-function(radar, basegrid_n,vprdir,refdir, radio_fold, radiosonde,..., nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv",mask=elevframe$id, z_min=0,z_max=3165,start=1, end=length(files),  minvprrange=5000, maxvprrange=20000,vprbottom=10,vprtop=1500, overwrite=T, sun=F, elev_min=0.5, elev_max=5,tol=10){
  #minvprrange=5000; maxvprrange=20000; vprbottom=10;vprtop=1500;overwrite=T
  require(foreign)
  require(radar)
  require(insol)
  require(RAtmosphere)
  print(paste0('currently making vprs'))
  ptm <- proc.time()
  
  ####get nexrad site parameters
  nexrad_get( radar=radar,nexrad_n=nexrad_n)
  ####read in basegrid
  
  basegridb<-read.dbf(basegrid_n)
  colnames(basegridb)<-tolower(colnames(basegridb))
  ra<-basegridb[,c('id','range','azimuth')]
  finalframe.all<-basegridb
  testgrid<-basegridb
  
  #############################
  #recalculate beam heights
  testgrid<-testgrid[testgrid$clutter==0,]
  testgrid<-cbind(testgrid, beamht_std(range=testgrid$range,groundht=testgrid$groundht,radar=radar,nexrad_n=nexrad_n))
  testgrid[,c('mbeam','bbeam','tbeam')]<-testgrid[,c('mbeam','bbeam','tbeam')]-testgrid$groundht
  ###list reflectivity files
  setwd(refdir)
  files<-list.files()
  
  ##################################################
  ### quick check to make sure every reflectivity file has a vpr file
  ### start reading through each file
  #if(missing('start')){start<-1}
  #if(missing('end')){end<-length(files)}
  finalist<-list()
  
  for(l in files[start:end]){
    vprlist<-list.files(vprdir)
    #l<-files[1]
    ###we'll read in the vpr first and leave it if need be
    radiosonde1<-radiosonde
    setwd(vprdir)
    check1<-sum(abs(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S') - as.POSIXlt(substr(vprlist,14,28),format='%Y%m%d-%H%M%S')) <time_diff(10,'min'))
    
    if( sum(check1)>0 &overwrite==F) { print(paste0(l,' already has a vpr made. Skipping to the next night'));next}
    
    #now to deal with the reflectivity
    ##############
    setwd(refdir)
    
    file<-read.csv(l)
    
    if(nrow(file)==0)stop(paste0('No rows in: ', l,'. Please check reflectivity file.'))
    
    time<-as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S', tz='UTC')
    ##this is the night we're refering too
    date1<-substr(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S', tz='UTC') - 60*60 *12,1,10)
    ##this is the actual night date (past midnight = next day)
    date2<-substr(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S', tz='UTC'),1,10)
    date3<-substr(as.POSIXlt(substr(l,26,40),format='%Y%m%d-%H%M%S', tz='UTC') + 60*60 *12,1,10)
    date_real<-paste0(substr(date1,1,4),substr(date1,6,7),substr(date1,9,10))
    date_file<-paste0(substr(date2,1,4),substr(date2,6,7),substr(date2,9,10))
    date_rad<-paste0(substr(date3,1,4),substr(date3,6,7),substr(date3,9,10))
    if(radiosonde==T){radioname<-paste0(radio_fold,'/','K',radar,'_',date_rad,'.htm')}
    interp_time<-substr(l,35,40)
    #colnames(file)<-c('azimuth','range','z_raw','elev') ###only for GL
    
    #####################################################################################
    ###a special 'if', depending on the radar and year, this vector of radars can & will grow
    if(as.numeric(substr(l,26,29))<=2011 & radar%in%c('DOX','MOB','LTX')){
      file$range <- file$range*1000+125 } else {
        file$range <- file$range*1000} 
    
    file$range <- round(file$range)
    file$azimuth <-  file$azimuth+0.25
    rangefloor <- min(file$range,na.rm=T)
    
    #######################################################
    ##   START vpr
    #if there is no associated radiosonde, it sets radiosonde to false
    setwd('C:')
    if( radiosonde1==FALSE||(file.info(radioname)$size<1000|is.na(file.info(radioname)$size<100)) ){radiosonde1<-FALSE
    print(paste0(l,' does not have a radiosonde, calculating with standard refraction'))}
    setwd(refdir)
    totala<-sort(unique(na.omit(file$elev))) ###for later calculations
    basegrid<-subset(testgrid, select=c(id,range,azimuth,pblock,clutter,groundht))
    test<-lapply(totala,function(x){merge(basegrid,file[file$elev==x,],by=c('range','azimuth'),all.x=T)})
    elevframe<-do.call(rbind,test)
    tt<-data.frame()
    for(i in 1:length(totala)){tt<-append(tt,rep(totala[i],each=lapply(test,nrow)[[i]]))}
    elevframe$elev<-unlist(tt)
    elevframe<-elevframe[,colnames(elevframe)%in%c('id','elev','range','azimuth','groundht','pblock','clutter','z_raw')]
    
    ###this is where we filter by zraw>1
    elevframe$z_raw[is.na(elevframe$z_raw)]<-0
    elevframe<-elevframe[elevframe$id%in%elevframe$id[elevframe$elev==0.5 & elevframe$z_raw>=z_min] ,]
    #mask=elevframe$id
    elevframe<-subset(elevframe, z_raw<z_max & clutter==0 & id %in% mask & elev>=elev_min & elev<=elev_max & z_raw>=0)
    
    if(SZA(time, Lat=lat, Lon=long)> 87 & SZA(time, Lat=lat, Lon=long)<93 & sun==T){
      sun_a<-sunpos(sunvector(JD(time), lat, long, 0))[1]
      elevframe<-subset(elevframe, !(azimuth < sun_a  + 1.25 & azimuth > sun_a - 1.25 ))
    }
    #tframe<-subset(tframe,clutter==0 | clutter==1 & pblock<.25, select = -c(clutter))  ###jeff wants me to do this but doesnt actually do it
    ##fill NA's with 0's and subset appropriately
    
    
    ###log transform before taking mean
    elevframe$lnz<-log(elevframe$z_raw+1)
    
    ######create moments table###########
    tryCatch(moments<-aggregate(elevframe, by=list(elevframe$elev,elevframe$range),FUN=mean) , error = function(err){print('Not all angles or ranges have data, check that the correct number of elevation angles are included, or that range and azimuth are being converted to match the basegrid appropriately ')})
    #moments<-aggregate(elevframe, by=list(elevframe$elev,elevframe$range),FUN=mean) ##currently not trimming
    moments$count<-aggregate(elevframe$lnz,by=list(elevframe$elev,elevframe$range),FUN=length)$x
    moments<-subset(moments,count>=3,select = -c(Group.1,Group.2))
    moments$id<-row.names(moments)
    moments$meanz<-exp(moments$lnz)-1 
    ##############################################################
    ##calculate beam heights
    ##############################################################
    range<-unique(moments$range)/1000
    setwd('C:')
    if(radiosonde1==T){
      print(paste0('using radiosonde: ','K',radar,'_',date_rad,'.htm', ' to calculate beams for: ',l))
      beamhtall<-beamht_radio(radio_n=radioname, radar=radar, total_elev=totala, range=range, nexrad_n=nexrad_n)
    }
    
    if(radiosonde1==F){
      beamhtall<-data.frame(range=moments$range,elev=moments$elev, beamht_std(range=moments$range,groundht=moments$groundht, radar=radar,elev=moments$elev,nexrad_n=nexrad_n))
    }
    setwd(refdir)
    frame1<-na.omit(merge(moments,beamhtall,by=c('range','elev'),all.x=T))
    
    frame1<-within(frame1, {bbeam <- bbeam - groundht
    mbeam <- mbeam - groundht
    tbeam <- tbeam - groundht})
    
    
    ###
    hghts=seq(vprbottom,vprtop,10)
    
    ###make a large dataframe of moments * height bins to do calculations all at once and calculate beam properties
    
    all<-do.call(rbind,lapply(hghts,function(x){frame1}))
    all$hght<-rep(hghts,each=nrow(frame1))
    all$id<-as.numeric(all$id)
    
    all<-beam_prop(data=all)
    all$meanpart<-all$partzh*all$meanz ###must be done seperately
    
    #####
    vprsub<-subset(all,range>=minvprrange & range<=maxvprrange)
    vpr1<-data.frame(z=as.numeric(by(vprsub$meanpart,vprsub$hght,sum,na.rm=T)/by(vprsub$partzh,vprsub$hght,sum,na.rm=T)),
                     hght=as.numeric(names(by(vprsub$meanpart,vprsub$hght,sum,na.rm=T))))
    ###fills in the bottom of the vpr
    if(min(which(!is.na(vpr1$z)))>1){vpr1$z[1:(min(which(!is.na(vpr1$z)))-1)]<-vpr1$z[ min(which(!is.na(vpr1$z)))] }
    vpr1$z<-round(vpr1$z,3)
    vpr1$z<-vpr1$z-min(vpr1$z>=125,na.rm=T)
    vpr1$z[is.na(vpr1$z)| vpr1$z<0]<-0
    ###creates the ratio of reflectivity
    vpr1$zh0<-(vpr1$z/mean(vpr1$z,na.rm=T))
    vpriter<-vpr1[,c('hght','zh0')]
    colnames(vpriter)[colnames(vpriter)%in%'zh0']<-'zh'
    #plot(vpr1$hght~vpr1$zh0)
    ####################################################################################
    ############ we made a vpr! #################################################
    ####################################################################################
    ###now we have to create a weighting variable that takes into account where the beam samples in each bin
    # 
    all<-merge(all,vpriter,by='hght',all.x=T)
    all$zhpart<-all$zh*all$partzh
    
    ##creates weighting variable to recalculate vpr with
    wt.row<-data.frame(id=as.numeric(aggregate(all$zhpart,by=list(all$id),sum,na.rm=T)$Group.1),vprwt=aggregate(all$zhpart,by=list(all$id),sum,na.rm=T)[,2]/aggregate(all$partzh,by=list(all$id),sum,na.rm=T)[,2])
    wt.row$vprwt<-round(wt.row$vprwt,1)
    
    ###fills in NA weights with 0
    wt.row$vprwt[is.na(wt.row$vprwt)]<-0 
    all<-merge(all,wt.row,by='id',all.x=T)
    
    ###corrects mean by the proper weight
    all$meanz2<-all$meanz*(all$zh/all$vprwt)
    all$meanz2[is.infinite(all$meanz2)]<-NA  ###fixes infinity
    
    ###now to recalculate vpr
    all$z2partzh<-all$meanz2*all$partzh
    all$proprow<-all$zh/sum(vpr1$zh)
    vprsub1<-subset(all,range>=minvprrange & range<=maxvprrange)
    vpr2<-data.frame(hght=as.numeric(names(by(vprsub1$z2partzh,vprsub1$hght,sum,na.rm=T))),
                     z3=as.numeric(by(vprsub1$z2partzh,vprsub1$hght,sum,na.rm=T)/by(vprsub1$partzh,vprsub1$hght,sum,na.rm=T)))
    
    ###fills in the bottom of the vpr
    
    vpr2$z3[1:(min(which(!is.na(vpr2$z3)))-1)]<-vpr2$z3[ min(which(!is.na(vpr2$z3)))] 
    
    ###filters out noise by subtracting the lowest value
    vpr2$z3<-(vpr2$z3-min(vpr2$z3[vpr2$hght>=125 &vpr2$z3>0],na.rm=T))
    vpr2$z3[is.na(vpr2$z3) | vpr2$z3<=0 |vpr2$z3>10000]<-0
    vpr2$zh1<-(vpr2$z3/mean(vpr2$z3,na.rm=T))
    
    vpr3<-merge(vpr1,vpr2,by='hght')[,c('hght','zh1')]
    vpr3$hght<-vpr3$hght/1000
    #plot(vpr2$hght~vpr2$zh1)
    
    ####we now have our vpr!
    #write.csv(vpr3,paste0(VPRDIR,'/','vpr_','K',q,'_',datereal,'.csv'))
    write.csv(vpr3,paste0(vprdir,'/','BirdVprTable_',date_file,'-',interp_time,'.csv'),row.names=F)
    ###############################clock#############################################
    total <- length(files)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, which(files==l))
    
    #################################################################################
    #######################
    #clean out some files
    rm(list=c('file','elevframe','moments','check1','date_file','date_rad','date_real','date1','date2','date3','interp_time','range','sun_a','time'))
    ##   end VPR
  }
} 