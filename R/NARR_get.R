#' Downloads lateast NARR wind data (North American Regional Reanalysis)
#' 
#' Currently only downloads wind data for the 1000mb - 600mb layer. If you'd like to download other variables, NCEP data from the RNCEP package should be adequate. 

#' This program only works when supplying specific radars which are detailed in the nexradmatch file. We don't currently have a program made that allows you to download based on lat/longs like the RNCEP package.
#' @param radar The 3 letter radar code
#' @param narr_dir The output directory for the summarized narr files
#' @param date_time The date and time of files you want downloaded, in the correct 3 hour segment and in as.POSIX date class
#' @param nexradmatch Location of a file of NARR cells that match with specified radar sites. Default location is on the AP's server
#' @param xy_n Location of a file of x and y coordinates of the narr files. Default location is on the AP's server
#' @param alternatesource Location of files later than December 2014 that have to be manually downloaded. Default location is on the AP's server.
NARR_get<-function(radar, narr_dir, date_time,...,nexradmatch="V:/Documents/RadarData/nexradmatch.dbf", xy_n="V:/Documents/RadarData/xy.csv",alternatesource="V:/Documents/RadarData/alternateNARR" ){
  
  require(foreign)
  require(rgdal)
  ###inner constant
  url<-c("http://nomads.ncdc.noaa.gov/data/narr/??????/????????/narr-a_221_????????_??00_000.grb")
  ###
  #nexmatch <- read.dbf(nexradmatch)
  nexmatch <- radar::nexradm
  #xy<-read.csv(xy_n) 
  xy<-radar::xy
  for( j in 1:length(date_time)){
    #j<-1
  n_times<-date_time[[j]]
  scrunch <- paste0(substr(n_times,1,4),substr(n_times,6,7),substr(n_times,9,10)) 
  substring(url,39) <- substr(scrunch,1,6)
  substring(url,46) <- scrunch
  substring(url,66) <- scrunch
  substring(url,75) <- sprintf("%02d", n_times$hour)
  print(url)
  if(file.exists(paste0(narr_dir,"/K",radar,"_",scrunch,"_",sprintf("%02d", n_times$hour),".csv",sep=""))){next}
  if (((n_times$mon+1)<10 & (n_times$year+1900)<=2014) |(n_times$year+1900)<=2014){
    
    download.file(url, paste0(narr_dir,'/',radar,"_", scrunch, ".grb"), mode="wb",extra=options(timeout = 180))
    narr<-readGDAL(paste0(narr_dir,'/',radar,"_", scrunch, ".grb"))}
  
  if (((n_times$mon+1)>=10 & (n_times$year+1900)==2014)|(n_times$year+1900)>2014){
    narr<-readGDAL(paste0(alternatesource,"/.",scrunch,sprintf("%02d", n_times$hour)))}
  
  narr<-narr[,,c(138,142,143,147,151,152,156,160,161,165,169,170,174,178,179,183,
                 187,188,192,196,197,201,206,207,211,215,216,220,224,225,229,233,
                 234,238,242,243,247,251,252,256,260,261,293,294,299,300)]
  narr<-as.data.frame(narr)
  names(narr)[1]="ht650mb"
  names(narr)[2]="u650mb"
  names(narr)[3]="v650mb"
  names(narr)[4]="ht700mb"
  names(narr)[5]="u700mb"
  names(narr)[6]="v700mb"
  names(narr)[7]="ht725mb"
  names(narr)[8]="u725mb"
  names(narr)[9]="v725mb"
  names(narr)[10]="ht750mb"
  names(narr)[11]="u750mb"
  names(narr)[12]="v750mb"
  names(narr)[13]="ht775mb"
  names(narr)[14]="u775mb"
  names(narr)[15]="v775mb"
  names(narr)[16]="ht800mb"
  names(narr)[17]="u800mb"
  names(narr)[18]="v800mb"
  names(narr)[19]="ht825mb"
  names(narr)[20]="u825mb"
  names(narr)[21]="v825mb"
  names(narr)[22]="ht850mb"
  names(narr)[23]="u850mb"
  names(narr)[24]="v850mb"
  names(narr)[25]="ht875mb"
  names(narr)[26]="u875mb"
  names(narr)[27]="v875mb"
  names(narr)[28]="ht900mb"
  names(narr)[29]="u900mb"
  names(narr)[30]="v900mb"
  names(narr)[31]="ht925mb"
  names(narr)[32]="u925mb"
  names(narr)[33]="v925mb"
  names(narr)[34]="ht950mb"
  names(narr)[35]="u950mb"
  names(narr)[36]="v950mb"
  names(narr)[37]="ht975mb"
  names(narr)[38]="u975mb"
  names(narr)[39]="v975mb"
  names(narr)[40]="ht1000mb"
  names(narr)[41]="u1000mb"
  names(narr)[42]="v1000mb"
  names(narr)[43]="u10m"
  names(narr)[44]="v10m"
  names(narr)[45]="u30m"
  names(narr)[46]="v30m"
  narr$ht10m=10
  narr$ht30m=30 
  #names(narr)[47]="x"
  #names(narr)[48]="y"
  #narr$x<-narr$x+1
  #narr$y<-narr$y+1
  narr$x<-xy[,1]+1  ##comment out previous four lines and uncomment this and 
  narr$y<-xy[,2]+1  ##this line if newer version of rgdal
  okayo<-merge(narr, nexmatch, by=c("x", "y"), all=T)
  okayo<-subset(okayo, okayo$SITE==radar)  
  file<-subset(data.frame(x=rep(okayo[,'x'],16), 
                          y=rep(okayo[,'y'],16), LAT_N=rep(okayo[,'LAT_N'],16), LONG_W=rep(okayo[,'LONG_W'],16), 
                          height=stack(okayo[,grep('ht',colnames(okayo))]), uwind=stack(okayo[,grep('u',colnames(okayo))]), 
                          vwind=stack(okayo[,grep('v', colnames(okayo))])), 
               select=c(x,y,LAT_N,LONG_W,height.values, uwind.values,vwind.values, height.ind))
  
  fname<-paste0("K",radar,"_",scrunch,"_",sprintf("%02d", n_times$hour),".csv",sep="")
  setwd(narr_dir)
  write.csv (file, fname, row.names=F)
  do.call(file.remove,list(list.files(pattern="\\.grb$")))
  }
}