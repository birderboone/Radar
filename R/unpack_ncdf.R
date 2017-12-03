# files='T:/Research_Projects/Sandy/radars/DOX/sandy_traffic/netcdf'
# radar='DOX'
# angle_max=5
# range_min=2125
# range_max=100000
# value='Reflectivity'
# par=F
# output='T:/Research_Projects/Sandy/radars/DOX/sandy_traffic/ref'

# library(devtools);load_all('C:/Users/radar/Desktop/Boone/r_packages/radar.0.2',recompile=T)
unpack_ncdf<-function(files,radar,output,...,angle_max=5,range_min=2125,range_max=100000,par=T,value='Reflectivity'){
  ##################
  re_azim<-function(data,old_azim, new_azim=seq(0,359.5,.5),r_bind=T){
    tt<-lapply(new_azim, function(z){
      az_a<-incomp(unique(old_azim),z)
      dd<-subset(data, old_azim==az_a)
      dd$azimuth<-z
      return(dd)
    })
    if(r_bind==T){df<-do.call(rbind, tt)}
    if(r_bind==F){df<-as.data.frame(tt)}
    return(df)
  }
  
  re_range<-function(data,old_range, new_range=seq(250,100000,250),r_bind=T){
    tt<-lapply(new_range, function(z){
      az_a<-incomp(unique(old_range),z)
      dd<-subset(data, old_range==az_a)
      dd$range<-z
      return(dd)
    })
    if(r_bind==T){df<-do.call(rbind, tt)}
    if(r_bind==F){df<-as.data.frame(tt)}
    return(df)
  }
  #################
  require(ncdf4)
  require(R.utils)
  require(parallel)
  require(raster)
  #ext='C:/Users/radar/Desktop/Boone/Radar/DOX/test'
  #radar='DOX'
  #angle_max=5
  #range_min=2125
  #range_max=100000
  #value='RhoHV'
  ##############################
  ext<<-files
  file.names<<-list.files(ext)
  
  
  #basegrid stuff
  #base<-read.dbf("C:/Users/radar/Desktop/Boone/basegrids/KDOX_100km_super_grid.dbf")
  #colnames(base)<-tolower(colnames(base))
  #base<-base[,c('id','range','azimuth')]
  #############
  #nexrad_get(radar)
  
  all_angle<<-file.names[as.numeric(file.names)<angle_max ]
  all_angle<<-all_angle[!is.na(all_angle)]
  
  times<<-lapply(all_angle,function(x)list.files(paste0(ext,'/',x)))
  lowfile<-list.files(paste0(ext,'/','00.50'))
  
  ###################################################################################
  # unparalleled
  ####################################################################################
  if(par==F){
  for(i in lowfile){
    #i<-lowfile[3]
    list1<-list()
    file_time<-as.POSIXlt(substr(i,1,15), format='%Y%m%d-%H%M%S') 
    angle_pos<-lapply(times,function(x){x[(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S') - file_time)>=0 & abs(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S') - file_time)<=time_diff(10,'min')]})
    
    angle_fil<-sapply(angle_pos, function(x) x[which.min(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S')-file_time)])
    
    sangle<-unlist(all_angle[sapply(angle_fil, function(x)sum(x>0)>0)])
    angle_fil<-unlist(angle_fil[sapply(angle_fil, function(x)sum(x>0)>0)])
    #sangle=all_angle[sapply(times, function(x)as.logical(sum(angle_fil%in%x)))]
    work.list<-data.frame(file=angle_fil,angle=sangle)
    #testlist[[i]]<-work.list}
    for(j in 1:nrow(work.list)){
      #j<-3
      #gunzip(paste0(ext,'/',work.list$angle[j],'/',work.list$file[j]),destname=paste0(ext,'/','temp.netcdf'),overwrite=T,remove=F)
      gunzip(paste0(ext,'/',work.list$angle[j],'/',work.list$file[j]),temporary=T,remove=F,overwrite=T)
      
      r.frame<-nc.open(paste0(tempdir(),'/',substr(work.list$file[j],1,22)))
    
    gtwd<-ncvar_get(r.frame,'GateWidth')[1]
    gate<-ncvar_get(r.frame,'Gate')
    aval<-data.frame(get.var.ncdf(r.frame,'Azimuth'))
    cval<-ncvar_get(r.frame,value)
    df.1<-as.data.frame(cval)
    rho1<-as.data.frame(cval)
    
    colnames(rho1)<-round(aval[,1]*4)/4
    
    row.names(rho1)<-gate*gtwd
    rho1<-rho1[,order(as.numeric(colnames(rho1)))]
    
    rho1[rho1==(-99900)]<-NA
    
    ang.df<-data.frame(range=rep(as.numeric(row.names(rho1),ncol(rho1))), 
      azimuth=as.numeric(rep(colnames(rho1),each=nrow(rho1))),value=unlist(rho1))
    ang.df<-subset(ang.df, range>=range_min & range<=range_max)
    if(length(unique(ang.df$azimuth))!=720){
      ang.df<-re_azim(ang.df, old_azim=ang.df$azimuth,new_azim=seq(0.25,359.75,.5))
    }
    dr<-unique(ang.df$range)
    dr<-dr[order(dr)]
    
    if(mean(diff(dr),na.rm=T)!=250){
      ang.df<-re_range(data=ang.df, old_range=ang.df$range,new_range=seq(250,range_max,250))
    }
    
    ang.df$time<-substr(work.list$file[j],1,15)
    ang.df$elev<-as.numeric(as.character(work.list$angle)[[j]])
    
    list1[[paste0(work.list$angle[j],'.',work.list$file[j])]]<-ang.df
    
    nc_open(r.frame)
    } ##end j loop

  final<-do.call(rbind, list1)
  colnames(final)[colnames(final)=='value']<-value

  
  write.csv(final,paste0(output,'/',radar,'_',value,'_',substr(i,1,15),'.csv') ,row.names=F)
  
  }  ##end i loop
  
  } #end par==F if
  
  ################################################################################
  ## Paralleled
  ################################################################################
  if(par==T){
    angle_max<<-angle_max
    range_min<<-range_min
    range_max<<-range_max
    
  cl<-makeCluster(detectCores())
  clusterSetRNGStream(cl)
  dumb<-clusterExport(cl, c('times','all_angle','ext','output','radar','angle_max',
                            'range_min',
                            'range_max',
                            'value'))
  junk<- clusterEvalQ(cl, list(require(R.utils), require(ncdf4)))
  parLapply(cl=cl,lowfile,function(i){
    #i<-lowfile[1]
    #i<-lowfile[1]
    #i<-lowfile[3]
    list1<-list()
    file_time<-as.POSIXlt(substr(i,1,15), format='%Y%m%d-%H%M%S') 
    angle_pos<-lapply(times,function(x){x[(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S') - file_time)>=0 & abs(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S') - file_time)<=time_diff(10,'min')]})
    
    angle_fil<-sapply(angle_pos, function(x) x[which.min(as.POSIXlt(substr(x,1,15), format='%Y%m%d-%H%M%S')-file_time)])
    
    sangle<-unlist(all_angle[sapply(angle_fil, function(x)sum(x>0)>0)])
    angle_fil<-unlist(angle_fil[sapply(angle_fil, function(x)sum(x>0)>0)])
    #sangle=all_angle[sapply(times, function(x)as.logical(sum(angle_fil%in%x)))]
    work.list<-data.frame(file=angle_fil,angle=sangle)
    for(j in 1:nrow(work.list)){
      #j<-3
      #gunzip(paste0(ext,'/',work.list$angle[j],'/',work.list$file[j]),destname=paste0(ext,'/','temp.netcdf'),overwrite=T,remove=F)
      gunzip(paste0(ext,'/',work.list$angle[j],'/',work.list$file[j]),temporary=T,remove=F,overwrite=T)
      
      r.frame<-nc_open(paste0(tempdir(),'/',substr(work.list$file[j],1,22)))
      
      gtwd<-ncvar_get(r.frame,'GateWidth')[1]
      gate<-ncvar_get(r.frame,'Gate')
      aval<-data.frame(ncvar_get(r.frame,'Azimuth'))
      cval<-ncvar_get(r.frame,value)
      df.1<-as.data.frame(cval)
      rho1<-as.data.frame(cval)
      
      colnames(rho1)<-round(aval[,1]*4)/4
      
      row.names(rho1)<-gate*gtwd
      rho1<-rho1[,order(as.numeric(colnames(rho1)))]
      
      rho1[rho1==(-99900)]<-NA
      
      ang.df<-data.frame(range=rep(as.numeric(row.names(rho1),ncol(rho1))), 
                         azimuth=as.numeric(rep(colnames(rho1),each=nrow(rho1))),value=unlist(rho1))
      ang.df<-subset(ang.df, range>=range_min & range<=range_max)
      if(length(unique(ang.df$azimuth))!=720){
        ang.df<-re_azim(ang.df, old_azim=ang.df$azimuth,new_azim=seq(0.25,359.75,.5))
      }
      dr<-unique(ang.df$range)
      dr<-dr[order(dr)]
      
      if(mean(diff(dr),na.rm=T)!=250){
        ang.df<-re_range(data=ang.df, old_range=ang.df$range,new_range=seq(250,range_max,250))
      }
      
      ang.df$time<-substr(work.list$file[j],1,15)
      ang.df$elev<-as.numeric(as.character(work.list$angle)[[j]])
      
      list1[[paste0(work.list$angle[j],'.',work.list$file[j])]]<-ang.df
      
      nc_close(r.frame)
    } ##end j loop
    
    final<-do.call(rbind, list1)
    colnames(final)[colnames(final)=='value']<-value
    
    
    write.csv(final,paste0(output,'/',radar,'_',value,'_',substr(i,1,15),'.csv') ,row.names=F)
    
  })
  stopCluster(cl)}
  removeTmpFiles(h=.01)
} ###end function