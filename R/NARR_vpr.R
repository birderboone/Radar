#' Downloads lateast NARR files (North American Regional Reanalysis) using the vpr files as a template.
#' 
#' Times are bracketed around the vprs time.
#' @param radar The 3 letter radar code
#' @param narr_dir The output directory for the summarized narr files
#' @param vprdir The file path of the vpr files (searches for the pattern: "VprTable_")
#' @param dates Override argument. A vector of the dates to consider in the vprfolder. In the format: yyyy-mm-dd
#' @param nexradmatch Location of a file of NARR cells that match with specified radar sites. Default location is on the AP's server
#' @param xy_n Location of a file of x and y coordinates of the narr files. Default location is on the AP's server
#' @param alternatesource Location of files for dates after December 2014 that have to be manually downloaded. Default location is on the AP's server.
NARR_vpr<-function(radar, narr_dir, vprdir,dates=vpr_dates,...,nexradmatch="V:/Documents/RadarData/nexradmatch.dbf", xy_n="V:/Documents/RadarData/xy.csv",alternatesource="V:/Documents/RadarData/alternateNARR" ){

##read in vpr files and dates
vpr_files<-list.files(vprdir, pattern="VprTable_")
vpr_dates<-substr(as.POSIXlt(substr(vpr_files,14,28), format='%Y%m%d-%H%M%S',tz='UTC') -12*60*60,1,10)
##filter if necessary, by default it will choose all dates

vpr_files<-vpr_files[vpr_dates %in% dates]

for(i in vpr_files){
  
#i<-vpr_files[1] 
date_name <- substr(as.POSIXlt(substr(i,14,28), format='%Y%m%d-%H%M%S',tz='UTC') -12*60*60,1,10)
time <- as.POSIXlt(substr(i,14,28), format='%Y%m%d-%H%M%S',tz='UTC')
time_early <- dtime_2_clock(substr(time,1,10), floor(to_dhour(time)/3)*3)
time_late <- as.POSIXlt((time_early + 3*60*60), format='%Y%m%d-%H%M%S',tz='UTC')
narr.times <- list(time_early,time_late)
NARR_get(radar=radar, narr_dir=narr_dir,date_time=narr.times, nexradmatch=nexradmatch, xy_n=xy_n, alternatesource=alternatesource)
} ###end i loop
} ###end function