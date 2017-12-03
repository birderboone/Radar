# library(foreign)
# library(devtools)
# install_local('C:/Users/Matt/Downloads/package/radar')
# library(radar)
# library(reshape2)
# 
# radar = 'GYX'
# basegrid_n = 'C:/Users/Matt/Downloads/package/DOX/KGYX_100km_super_grid_UTM.dbf'
# nexrad_n = 'C:/Users/Matt/Downloads/package/nexrad_site_list_with_utm.csv'
# angle = 0.5
# basegrid<-read.dbf(basegrid_n)
# colnames(basegrid)<-tolower(colnames(basegrid))
########

#' Calculates the percentage of beam blockage across a radar range
#' 
#' This function requires input of a dataframe with three columns 'range','azimuth', and 'groundht'
#' @param data  data.frame with numberic columns for range, azimuth, and ground height (groundht)
#' @param radar the 3 letter name  of the radar
#' @param angle  the angle of the beam can either be a scalar or vector
#' @param nexrad_n  location of the nexrad site table. Default is the location on the AP's server
beam_block<-function(data,radar, angle=0.5, nexrad_n='V:/Documents/nexrad_site_list_with_utm.csv'){
  require(radar)

data <- basegrid
nexrad_get(radar, nexrad_n)

beam<-beamht_std(data$range, antht=antht, elev=angle, earthr=earthr)
beam[,c('bbeam','mbeam','tbeam')] <- beam[,c('bbeam','mbeam','tbeam')] - data$groundht

beam$block<-0

beam$block[beam$tbeam<=0]<-1

beam$block[beam$bbeam<0 & beam$mbeam >=0 ] <-mround(-1*beam$bbeam[beam$bbeam<0 & beam$mbeam >=0 ]/(2*beam$range[beam$bbeam<0 & beam$mbeam >=0 ]*halfbeam*cos(deg2rad*angle)), 0.05)

beam$block[beam$bbeam <0 & beam$mbeam <0 ] <- mround(1-(beam$tbeam[beam$bbeam <0 & beam$mbeam <0 ]/(2*beam$range[ beam$bbeam <0 & beam$mbeam <0 ]*halfbeam*cos(deg2rad*angle))), 0.05)

beam$azimuth <- data$azimuth

beam <- beam[order(beam$azimuth, beam$range),]

list1<-tapply(beam$block, beam$azimuth, list)

here<-lapply(list1,  function(x) {
  listblock<-list()
  for(i in 1:length(x)){
    if(i==1){pblock<-0}
    pblock<-max(c(x[i],pblock))
    if(pblock<0){pblock<-0}
    listblock[[i]]<-pblock
  }
  listblock
})


beam$pblock<-unlist(here)
beam
}

##this is for testing if it works against an already calculated pblock
# result<-merge(data, beam, by=c('range','azimuth'))
# result1<-result[,c('id','pblock.x','pblock.y')]
# bad<-subset(result1,mround(pblock.x,.05)!=mround(pblock.y,.05))
# bad_d<-result[result$id%in%bad$id,]
# bad_d<-bad_d[order(bad_d$azimuth, bad_d$range),]
# cor(result1[,2:3])
