###calculates beam properties given the top of the beam, middle beam, pblock, range, elev, and zh1, which has been created earlier
#this function has been replaced by the more universal beam_prop function 
beam_prop_vpr<-function(data,totalzh){
  
  ##make a table of missing values
    test2<-data
    test2$maxbin<-test2$hght
    test2$cenbin<-test2$hght-5
    test2$minbin<-test2$hght-10
  
  ##########################################
  
  
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
                                                                                                                                                    cos(deg2rad*workframe$elev[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])))
  
  workframe$alphamin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]<-
    acos((workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]-
            workframe$minbin[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)])/((workframe$mbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)]-
                                                                                                                                                        workframe$bbeam[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & !(workframe$minbin >= workframe$mbeam)])*
                                                                                                                                                       cos(deg2rad*workframe$elev[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])))
  
  ###take a quick break
  ##this should technically be the same except 'min' is replaced with 'max'
  workframe$alphamax[workframe$maxbin<=workframe$bbeam]<-0
  workframe$alphamax[workframe$maxbin>=workframe$tbeam]<-pi
  workframe$alphamax[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]<-
    acos((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]-
            workframe$maxbin[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam])/((workframe$tbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam]-
                                                                                                                                                     workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & workframe$maxbin >= workframe$mbeam])*
                                                                                                                                                    cos(deg2rad*workframe$elev[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])))
  
  workframe$alphamax[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]<-
    acos((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]-
            workframe$maxbin[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)])/((workframe$mbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)]-
                                                                                                                                                        workframe$bbeam[!(workframe$maxbin<=workframe$bbeam) & !(workframe$maxbin>=workframe$tbeam) & !(workframe$maxbin >= workframe$mbeam)])*
                                                                                                                                                       cos(deg2rad*workframe$elev[!(workframe$minbin<=workframe$bbeam) & !(workframe$minbin>=workframe$tbeam) & workframe$minbin >= workframe$mbeam])))
  workframe$alphamax[is.na(workframe$alphamax)]<-NA ####sets NANs to NA 
  workframe$alphamin[is.na(workframe$alphamin)]<-NA ####sets NANs to NA
  ###it's like magic
  
  workframe$gstd<-.85*((workframe$tbeam-workframe$mbeam)/sqrt(2))  ###used to be divided by sqrt(2)
  workframe$gpeak<-(1/sqrt(2*pi*workframe$gstd*workframe$gstd))
  workframe$gpdf <- (1/(sqrt(2*pi)*workframe$gstd))*exp(-1*(workframe$mbeam-workframe$cenbin)*(workframe$mbeam-workframe$cenbin)/(2*workframe$gstd^2))
  workframe$coeff = (workframe$gpdf/workframe$gpeak)*((1/pi)*((workframe$alphamax-workframe$alphamin)-((sin(workframe$alphamax)*cos(workframe$alphamax))-(sin(workframe$alphamin)*cos(workframe$alphamin)))))
  workframe$partzh = workframe$zh1*workframe$coeff
  workframe$part = (1/pi)*((workframe$alphamax-workframe$alphamin)-((sin(workframe$alphamax)*cos(workframe$alphamax))-(sin(workframe$alphamin)*cos(workframe$alphamin))))/(1-workframe$pblock)
  workframe
}