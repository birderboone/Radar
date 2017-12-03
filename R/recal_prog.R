#does a rough calculation of correction factor given  a data frame with the top of the beam, middle beam, bottom of the beam, pblock, range, and groundht

###TROUBLESHOOTING
# data=recalframe
# vpr=vpr3
###
#' Calculates the correction factor for a specific range, groundht, and pblock. 
#' 
#' Used primarily in the summar code and may not be very useful as a stand alone program. Calculated using standard refraction.
#' @param data A dataframe of all necessary information needed for calculation. Includes 6 columns: tbeam = top of the beam, mbeam = middle of the beam, bbeam = bottom of the beam, pblock = percent of beam blocked, range = range from radar in meters, groundht = ground height above sealevel.
#' @param vpr A dataframe of the VPR. Includes 2 columns: hght = height in meters, zh1 = the calculated vpr value
recal_corr<-function(data, vpr,elev){
  
  recalframe<-data

###
recalframe<-recalframe[,c('tbeam','mbeam','bbeam','pblock','range','groundht')]
recalframe$bbeam<-round(recalframe$tbeam-((1-recalframe$pblock)*2*recalframe$range*halfbeam*cos(deg2rad*elev)),0)
recalframe1<-unique(recalframe)
recalframe1$id<-1:nrow(recalframe1)

test1<-mapply(function(x,y){subset(vpr,vpr$hght>=x & (vpr$hght-10)<=y)},recalframe1$bbeam, recalframe1$tbeam,SIMPLIFY=T)
test2<-as.data.frame(apply(test1,1,unlist))
len<-unlist(lapply(test1[1,],length))
test2$bbeam<-rep(recalframe1$bbeam,len)
test2$tbeam<-rep(recalframe1$tbeam,len)
test2$mbeam<-rep(recalframe1$mbeam,len)
test2$pblock<-rep(recalframe1$pblock,len)
test2$id<-rep(recalframe1$id,len)
test2$maxbin<-test2$hght
test2$cenbin<-test2$hght-5
test2$minbin<-test2$hght-10
##########################################
elev<-elev
totalzh<-sum(vpr$zh1)
######prepare for battle##################
workframe<-test2
workframe$alphamin<-NA
workframe$alphamax<-NA
###########################################
workframe$alphamin[workframe$minbin<=workframe$bbeam]<-0

workframe$alphamin[!(workframe$minbin<=workframe$bbeam) & workframe$minbin>=workframe$tbeam]<-pi

workframe$alphamin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam]<-
  acos((workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam]-
          workframe$minbin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])/((workframe$tbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam]-
                                                                                                                                                   workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])*
                                                                                                                                                  cos(deg2rad*elev)))

workframe$alphamin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]<-
  acos((workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]-
          workframe$minbin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)])/((workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]-
                                                                                                                                                      workframe$bbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)])*
                                                                                                                                                     cos(deg2rad*elev)))

###take a quick break
##this should technically be the same except 'min' is replaced with 'max'
workframe$alphamax[workframe$maxbin<=workframe$bbeam]<-0
workframe$alphamax[workframe$maxbin>=workframe$tbeam]<-pi
workframe$alphamax[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]<-
  acos((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]-
          workframe$maxbin[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam])/((workframe$tbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]-
                                                                                                                                                   workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam])*
                                                                                                                                                  cos(deg2rad*elev)))

workframe$alphamax[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]<-
  acos((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]-
          workframe$maxbin[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)])/((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]-
                                                                                                                                                      workframe$bbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)])*
                                                                                                                                                     cos(deg2rad*elev)))
workframe$alphamax[is.na(workframe$alphamax)]<-NA ####sets NANs to NA 
workframe$alphamin[is.na(workframe$alphamin)]<-NA ####sets NANs to NA
###jesus it's like magic

workframe$gstd<-.85*((workframe$tbeam-workframe$mbeam)/sqrt(2))  ###used to be divided by sqrt(2)
workframe$gpeak<-(1/sqrt(2*pi*workframe$gstd*workframe$gstd))
workframe$gpdf <- (1/(sqrt(2*pi)*workframe$gstd))*exp(-1*(workframe$mbeam-workframe$cenbin)*(workframe$mbeam-workframe$cenbin)/(2*workframe$gstd^2))
workframe$coeff = (workframe$gpdf/workframe$gpeak)*((1/pi)*((workframe$alphamax-workframe$alphamin)-((sin(workframe$alphamax)*cos(workframe$alphamax))-(sin(workframe$alphamin)*cos(workframe$alphamin)))))
workframe$partzh = workframe$zh1*workframe$coeff
workframe$part = (1/pi)*((workframe$alphamax-workframe$alphamin)-((sin(workframe$alphamax)*cos(workframe$alphamax))-(sin(workframe$alphamin)*cos(workframe$alphamin))))/(1-workframe$pblock)
#workframe$proprow <- workframe$zh1/totalzh*100

####
  #workframe$proprow <- workframe$zh1/totalzh*100
  if(sum(!(recalframe1$id%in%workframe$id)>0)){stop('vpr does not have enough height bins calculated to match with the reflectivity. Rerun VPRS with higher/lower limits')}
recalframe1$za<-aggregate(workframe$partzh,by=list(workframe$id),sum,na.rm=T)[,2]/aggregate(workframe$coeff,by=list(workframe$id),sum,na.rm=T)[,2]
recalframe1$propdist<-aggregate(workframe$zh1/sum(vpr$zh1),by=list(workframe$id),sum,na.rm=T)[,2]

  recalframe1<-recalframe1[,c('range','groundht','pblock','za', 'propdist')] ##includes propdist
  recalframe1
  } ###end recal

