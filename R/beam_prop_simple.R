#this data frame needs to have hght (bins), bbeam (bottom of beam), tbeam (top of beam), mbeam (middle of beam), and elevation
#This is a universal function, that will calculate beam properties for a dataframe

#data=test2
beam_prop<-function(data){
  all<-data
all$maxbin<-all$hght
all$minbin<-all$hght-10
all$cenbin<-all$hght-5
all$alphamin<-0
all$alphamax<-0
#all$bbeam<-round(all$bbeam,2)
#all$mbeam<-round(all$mbeam,2)
#all$tbeam<-round(all$tbeam,2)
################################################################
######NANs will be produced in this when the beam is < 1m in the bin, do not panic

all$alphamin[all$minbin<=all$bbeam]<-0

all$alphamin[!(all$minbin<=all$bbeam) & all$minbin>=all$tbeam]<-pi

all$alphamin[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam]<-
  acos((all$mbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam]-
          all$minbin[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam])/((all$tbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam]-
                                                                                                         all$mbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam])*
                                                                                                        cos(deg2rad*all$elev[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & all$minbin >= all$mbeam])))

all$alphamin[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)]<-
  acos((all$mbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)]-
          all$minbin[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)])/((all$mbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)]-
                                                                                                            all$bbeam[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)])*
                                                                                                           cos(deg2rad*all$elev[!(all$minbin<=all$bbeam) & !(all$minbin>=all$tbeam) & !(all$minbin >= all$mbeam)])))

###take a quick break
##this should technically be the same except 'min' is replaced with 'max'
all$alphamax[all$maxbin<=all$bbeam]<-0
all$alphamax[all$maxbin>=all$tbeam]<-pi
all$alphamax[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam]<-
  acos((all$mbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam]-
          all$maxbin[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam])/((all$tbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam]-
                                                                                                         all$mbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam])*
                                                                                                        cos(deg2rad*all$elev[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & all$maxbin >= all$mbeam])))

all$alphamax[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)]<-
  acos((all$mbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)]-
          all$maxbin[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)])/((all$mbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)]-
                                                                                                            all$bbeam[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)])*
                                                                                                           cos(deg2rad*all$elev[!(all$maxbin<=all$bbeam) & !(all$maxbin>=all$tbeam) & !(all$maxbin >= all$mbeam)])))
all$alphamax[is.na(all$alphamax)]<-NA ####sets NANs to NA 
all$alphamin[is.na(all$alphamin)]<-NA ####sets NANs to NA
###jesus it's like magic

all$gstd<-.85*((all$tbeam-all$mbeam)/sqrt(2))  ###used to be divided by sqrt(2)
all$gpeak<-(1/sqrt(2*pi*all$gstd*all$gstd))
all$gpdf <- (1/(sqrt(2*pi)*all$gstd))*exp(-1*(all$mbeam-all$cenbin)*(all$mbeam-all$cenbin)/(2*all$gstd^2))
all$coeff<-(all$gpdf/all$gpeak)*((1/pi)*((all$alphamax-all$alphamin)-((sin(all$alphamax)*cos(all$alphamax))-(sin(all$alphamin)*cos(all$alphamin)))))
all$part = (1/pi)*((all$alphamax-all$alphamin)-((sin(all$alphamax)*cos(all$alphamax))-(sin(all$alphamin)*cos(all$alphamin))))
all$partzh = (all$gpdf/all$gpeak)*((1/pi)*((all$alphamax-all$alphamin)-((sin(all$alphamax)*cos(all$alphamax))-(sin(all$alphamin)*cos(all$alphamin)))))

all
}