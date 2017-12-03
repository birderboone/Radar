#' Calculates the a summary table using regression on order statistics for a radar
#' 
#' This program inputs the summary file created from summary(). Technically you just need a data.frame with id, range, clutter, waterfilt, and all nights with column names 'zyymmdd'. The file will automatically be named K'radar'_ros_'folder'_year. So the folder is the main vector for changing file names with successive runs. You can input a paste0() function if looping through different inputs and would like to change the out put name
#' @param radar The 3 letter radar code
#' @param folder The project name that will be tacked onto the name of the files
#' @param basegrid_n The location of the basegrid
#' @param out_summary File path for the summary file created from summary()
#' @param out_ros Folder to save the output ROS summary file
#' @param ea This is the fabled 'eagle file'. It is a seldom used argument that creates a large file with all nights, summarized every year, and all ID fields. It gets large fast and takes a while to run.
#' @param total Do you want to run a summary combining all years
#' @param seperate Do you want to run a summary each year
#' @param date_mask A vector of dates that the program will subset from the summary file. Must be in 'yyyy-mm-dd' format.
#Rros_exodus<-function(radar, folder, basegrid_n, out_summary, ea=F, total=T, seperate=F){
  ros_exodus<-function(radar, folder,...,basegrid_n,out_summary,out_ros,ea=F, total=T, seperate=F,date_mask=as.POSIXlt(colnames(rosdataall)[grepl('^z',colnames(rosdataall))], format='z%y%m%d')){  
  require(NADA)
  require(parallel)
  require(foreign)
    require(radar)
  if(ea==TRUE) {basegridb<-read.dbf(basegrid_n)
  colnames(basegridb)<-tolower(colnames(basegridb))}
  ##################################################
  
  #################################################
  ptm<-proc.time()
  #setwd(out_summary)
  rosdataall=read.csv(out_summary)
  #rosdataall<-rosdataall[!(apply(apply(rosdataall[,grepl('^z',colnames(rosdataall))],2,is.na),1,sum)>=(sum(grepl('^z',colnames(rosdataall)))-1)),]
  #rosdataall<-rosdataall[1:5000,]
  ######date mask
  colnames(rosdataall)<-tolower(colnames(rosdataall))
  
  zmask<-date_mask
  date_maskz<-paste0('z',substr(zmask,3,4), substr(zmask,6,7), substr(zmask,9,10))

  rosdataall<-cbind(rosdataall[,!grepl('^z',colnames(rosdataall))], rosdataall[,colnames(rosdataall)%in%date_maskz])
  
  ID<-matrix(ncol=1,rosdataall[,'id'])
  
  ###################################################
  ##
  uniquerange<-unique(rosdataall$range)
  uniquerange<-uniquerange[order(uniquerange)]
  bingrid<-uniquerange[2]-uniquerange[1]
  #min(rosdataall$range[rosdataall$range!=(is.na(rosdataall))])
  ifelse(min(uniquerange)>2125,rgate<-min(uniquerange),rgate<-2125)
  uniquerange<-uniquerange[uniquerange>=rgate]
  basegridcol<-min(grep('^z',colnames(rosdataall)))-1
  ##
  datecolnames<-grep('^z',colnames(rosdataall))
  zdates<-colnames(rosdataall)[grep('^z',colnames(rosdataall))]
  dates<-substr(zdates,2,nchar(zdates))
  years<-paste0('20',substr(dates,1,2))
  years<-unique(years)
  roslist<-list()
  ############
  ##define whats in the worklist
  if(seperate==TRUE & total==TRUE){worklist<-c(1:length(years),'all')}
  if(seperate==FALSE & total==TRUE){worklist<-'all'}
  if(seperate==TRUE & total==FALSE){worklist<-c(1:length(years))}
  #worklist<-c(which(years=='2012'),'all')  ###manual overwrite
  ###################################################
  #worklist<-1
  
  for(p in worklist){
    #p<-worklist[1]
    if(!(is.na(as.numeric(p)))){mess<-years[as.numeric(p)]}else{mess<-p}
    print(paste0("don't panic, there will be no status bar. We are currently running ROS for ",mess))
    if(p=='all'){min<-min(datecolnames)
    max<-max(datecolnames)}
    if(p!='all'){min<-min(grep(paste0('z',substr(years[as.numeric(p)],3,4)),colnames(rosdataall)))
    max<-max(grep(paste0('z',substr(years[as.numeric(p)],3,4)),colnames(rosdataall)))}
    
    ptm<-proc.time()
    #ID<-matrix(ncol=1,rosdataall[,'ID'])
    #colnames(ID)<-'ID'
    
    ###figure out the minimum detection value for each range and day combination
    rosdata2=subset(rosdataall, clutter==0 & range >=2125)
    
    rosdata2=cbind(rosdata2[,min:max],rosdata2[,c("id","range")])
    rosdata2<-rosdata2[apply(is.na(rosdata2[,1:length(min:max)]),1,sum)!=(length(min:max)-1),]
    nondetect<-rosdata2[,1:length(min:max)]==0
    nondetect[is.na(nondetect)]<-FALSE
    nondetect2<-nondetect
    #nondetect2<-nondetect2[apply(is.na(rosdata2[,1:length(min:max)]),1,sum)!=(length(min:max)-1),]
    
    
    rosdata3<-rosdata2[apply((is.na(rosdata2[,1:length(min:max)]) | nondetect),1,sum)<=(length(min:max)-2),]
    nondetect<-nondetect[apply((is.na(rosdata2[,1:length(min:max)]) | nondetect),1,sum)<=(length(min:max)-2),]
    ##replace zeros with minimum detect level
    rosdata3[,1:length(min:max)][nondetect]<-rep(do.call(rbind,apply(rosdata3[,1:length(min:max)],2,function(x){aggregate(x,by=list(rosdata3$range),
                                                                                                                          function(y){min(y[y!=0],na.rm=T)})}))$x,do.call(rbind,apply(rosdata3[,1:length(min:max)],2,function(x){aggregate(x,by=list(rosdata3$range),length)}))$x)[nondetect]
    ##filter out infitiy, values, and nights that have only one observation or none
    rosdata3[rosdata3>10000000000]<-NA
    rosdata4<-rosdata3[apply((is.na(rosdata3[,1:length(min:max)]) | nondetect),1,sum)<=(length(min:max)-2),]
    nondetect<-nondetect[apply((is.na(rosdata3[,1:length(min:max)]) | nondetect),1,sum)<=(length(min:max)-2),]
    rosdata3<-rosdata4
    ###################
    here1<-apply(rosdata3[,1:length(min:max)],1,list)
    where1<-apply(as.data.frame(nondetect),1,list)
    ##get your parallel computing on, time do the real business
    print("I'm starting paralleling")
    ###################
    ##get your parallel computing on, time do the real business
    setwd('C:')
    rangeoutr<-list()
    cl<-makeCluster(detectCores())
    clusterSetRNGStream(cl)
    #dumb<-clusterExport(cl, c('here1','where1'))
    junk<- clusterEvalQ(cl, require(NADA))
    rangeoutr <- clusterMap(cl=cl, dumb, here1, where1)
    stopCluster(cl)
    
    print("I'm done paralleling")
    
    rangeoutall<-data.frame(do.call(rbind,rangeoutr))
    colnames(rangeoutall)<-c("ROSGEO","ROSMEAN","ROSMED","CVROS","PCENS","ROSN")
    
    remaining<-merge(rosdata2[apply((is.na(rosdata2[,1:length(min:max)]) | nondetect2),1,sum)>(length(min:max)-2),],data.frame(0,0,0,NA),all.x=T)
    if(nrow(remaining)>0){row.names(remaining)<-remaining$id
    remaining$PCENSORED<-(apply(remaining[,grep('^z',colnames(remaining))]==0,1,sum,na.rm=T))/(apply(!is.na(remaining[,grep('^z',colnames(remaining))]),1,sum))
    remaining$ROSN<-(apply(!is.na(remaining[,grep('^z',colnames(remaining))]),1,sum))
    remaining<-remaining[,(ncol(remaining)-5):ncol(remaining)]
    colnames(remaining)<-c("ROSGEO","ROSMEAN","ROSMED","CVROS","PCENS","ROSN")
    rangeoutall<-rbind(rangeoutall,remaining)}
    rangeoutall$id<-row.names(rangeoutall)
    #rangeoutall<-merge(rosdataall[rangeoutall$id,],rangeoutall,by.x='ID',by.y='id',all.x=T)
    rangeoutall$TOTALN<-max(rangeoutall$ROSN,na.rm=T)
    setwd(out_ros)
    if(p!='all' & ea==TRUE){write.dbf (merge(basegridb,rangeoutall,by='id',all.y=T),paste0('K',radar,'_',folder,'_ros_',years[as.numeric(p)],'.dbf'))}
    if(p!='all' & ea!=TRUE){write.dbf (merge(rosdataall[,c(colnames(rosdataall)[-grep('^z',colnames(rosdataall))],colnames(rosdataall)[min:max])],rangeoutall,by='id',all.y=T),paste0('K',radar,'_ros_',folder,'_',years[as.numeric(p)],'.dbf'))}
    if(p!='all'){zname<-paste0(substr(years[as.numeric(p)],3,4))}
    if(p=='all'){zname<-'al'}
    #paste0(zname,'_',substr(colnames(rosdataall[,min:max]),6,9))
    
    roslist[[zname]]=subset(merge(data.frame(id=rosdataall[,'id']),rangeoutall,by='id',all.x=T),select = -id) 
  } #end p loop
  final<-cbind(rosdataall[,1:max(datecolnames)],do.call(cbind,roslist))
  
  
  #final[is.na(final)]<-(-99900)
  setwd(out_ros)
  if(ea==TRUE){write.dbf (final,paste0('K',radar,'_',folder,'_grandsummary.dbf'))}
  if(total==TRUE){write.dbf (final[!is.na(final$al.ROSGEO),],paste0('K',radar,'_ros_',folder,'_all.dbf'))}
  
  #write.dbf(rosdatafinal,'C:/Users/radar/Desktop/Boone/Radar/DOX/validation/KDOX_validation_ros_test.dbf')
  proc.time() - ptm
}