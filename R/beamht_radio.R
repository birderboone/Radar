#' Calculates the height of the radar beam using radiosondes
#' 
#' Output is a list containing the bottom, middle, and top of the beam.  
#' Use beamht_std if you want to calculate beam heights using standard refraction
#' @param radio_n  file path of the radiosonde in .html
#' @param radar  the 3 letter radar code
#' @param range numeric range of each pulse volume 
#' @param total_elev a vector of the unique elevations you want to calculate beam heights for
#' @param nexrad_n  location of the nexrad site table. Default is the location on the AP's server

beamht_radio<-function(radio_n, radar, range,total_elev, nexrad_n='V:/Documents/nexrad_site_list_with_utm.csv'){
  #radio<-radio
  #require(radar)
  radio<-read.fwf(radio_n, widths=c(7,7,7,7,7,7,7,7,7,7,7), sep="t", skip=10, n=30, na.strings=c("       "))
  colnames(radio)<-tolower(substr(t(read.fwf(radio_n, widths=c(7,7,7,7,7,7,7,7,7,7,7), sep="t", skip=7,n=1)[1,]),4,7))
  nexrad_get(nexrad_n=nexrad_n,radar=radar)
  beamhtall<-data.frame(range=numeric(),bbeam=numeric(),mbeam=numeric(),tbeam=numeric(),elev=numeric())
  ###calculate refraction
  
  radio$hght<-radio$hght-baset
  radio$hght[radio$hght<=0]<-1
  radio<-radio[radio$hght>=0,]
  
  radio<-radio[max(which(radio$hght<antht)):nrow(radio),]
  radio<-aggregate(radio,by=list(radio$hght),mean)[,2:ncol(radio)]
  
  radio$watervapor<-6.1078*10**((radio$dwpt*7.5)/(radio$dwpt+237.3))
  radio$refractivity <- (77.6/(radio$temp+273.18))*(radio$pres+(4810*(radio$watervapor/(radio$temp+273.18))))
  radio$n<- 1+(radio$refractivity/1000000)
  n1<-c(0,radio$n[1:nrow(radio)-1])
  radio$hght[radio$hght==antht]<-radio$hght[radio$hght==antht]-1
  radio$heightb<-c(0,radio$hght[1:nrow(radio)-1])
  radio$refindex<-(radio$n-n1)/((radio$hght-radio$heightb)/1000)
  radio$theta<-0
  radio$arcdist<-NA
  radio$a<-0
  radio$k<-NA
  radio<-radio[!is.na(radio$n),]
  ################################################################
  ###loop through top and bottom angles
  
  for(elev in total_elev){
    #elev<-total_elev[1] ###only for test purposes
    testht<-data.frame(range=range,bbeam=NA,mbeam=NA,tbeam=NA,elev=elev)
    
    for(beam in c(elev*deg2rad-halfbeam,elev*deg2rad,elev*deg2rad+halfbeam)){
      #beam<-elev*deg2rad-halfbeam ###only for test purposes
      mom<-which(beam==c(elev*deg2rad-halfbeam,elev*deg2rad,elev*deg2rad+halfbeam))
      radioframe<-radio
      ################################################################
      #####
      ####if height<antht
      radioframe$a[radioframe$hght<antht]<-(eartha/1000)
      radioframe$theta[radioframe$hght<antht]<-((beam)) ###dependent on part of beam
      radioframe$thickness[radioframe$hght<antht]<-0
      radioframe$arcdist[radioframe$hght<antht]<-0
      ################################################################
      
      ################################################################
      ####if heightb<antht
      radioframe$thickness[radioframe$hght>antht & radioframe$heightb<antht]<-(radioframe$hght[radioframe$hght>antht & radioframe$heightb<antht]-antht)/1000
      radioframe$theta[radioframe$hght>antht & radioframe$heightb<antht]<-(beam)
      
      ################################################################
      ####otherwise
      radioframe$thickness[radioframe$heightb>antht]<-(radioframe$hght[radioframe$heightb>antht]-radioframe$heightb[radioframe$heightb>antht])/1000
      radioframe$a<-c(rep(eartha/1000,max(which(!(radioframe$hght>antht)))),eartha/1000+cumsum(radioframe$thickness)[which(radioframe$hght>antht)-1])
      a1<-c(0,radioframe$a[1:nrow(radioframe)-1])  ###lag a
      radioframe$thetap[a1*radioframe$refindex<=(-1)]<-asin(sqrt((-2)*(radioframe$thickness[a1*radioframe$refindex<=(-1)])*(1+(radioframe$refindex[a1*radioframe$refindex<=(-1)]*a1[a1*radioframe$refindex<=(-1)]))/(a1[a1*radioframe$refindex<=(-1)])))
      radioframe$thetap[!(a1*radioframe$refindex<=(-1))&radioframe$heightb>antht]<-0
      ################################################################
      ###more otherwise
      rt<-which(radioframe$hght>antht) 
      rtm<-max(which(!(radioframe$hght>antht)))
      
      for(i in which(radioframe$heightb>antht)){  radioframe$theta[i]<-atan(sqrt((((radioframe$a[i-1])*sin(radioframe$theta[i-1]))**2)+(2*radioframe$a[i-1]*(radioframe$thickness[i-1])*(1+(radioframe$refindex[i-1]*radioframe$a[i-1]))))/radioframe$a[i-1]*cos(radioframe$theta[i-1]))}
      
      radioframe$arcdist[rt]<-(cos(radioframe$theta[rt])/(1+(radioframe$refindex[rt]*radioframe$a[rt])))*(sqrt(((radioframe$a[radioframe$hght>antht]*sin(radioframe$theta[rt]))**2)+(2*radioframe$a[rt]*(1+(radioframe$refindex[radioframe$hght>antht]*a1[rt]))*radioframe$thickness[rt]))-(a1[rt]*sin(radioframe$theta[rt])))
      radioframe$arcdist[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap]<-2*((-radioframe$a[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap]*cos(radioframe$theta[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap])*sin(radioframe$theta[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap]))/((radioframe$refindex[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap]*radioframe$a[radioframe$thetap>0 & radioframe$t1<=radioframe$thetap])+1))
      radioframe$k[radioframe$hght>antht]<-(1/(1+(a1[radioframe$hght>antht]*radioframe$refindex[radioframe$hght>antht])))
      
      ###############################################################
      
      ####make ranges####
      
      radioframe$cumrange<-cumsum(radioframe$arcdist)
      radioframe$prevrange<-c(0,cumsum(radioframe$arcdist)[1:nrow(radioframe)-1])
      radioframe$prevrange[is.na(radioframe$prevrange)]<-0
      
      ##############################################################
      ####now we're creating beam heights
      bb<-radioframe[sapply(range,function(x){min(which(x<=radioframe$cumrange))}),]
      bb$range<-range
      bb$relrange<-bb$range-bb$prevrange
      testht[,1+mom]<-bb$k*bb$a*((cos(bb$theta)/cos(bb$theta+(bb$relrange/(bb$k*bb$a))))-1)*1000+(bb$hght-(bb$thickness*1000))
      
    }###end beam loop
    beamhtall<-rbind(beamhtall,testht)
  }###end elev loop
  ####fix ranges
  
  beamhtall$range<-beamhtall$range*1000
  beamhtall$bbeam <- beamhtall$bbeam + antht
  beamhtall$mbeam <- beamhtall$mbeam + antht
  beamhtall$tbeam <- beamhtall$tbeam + antht

  
  ##check if there is strong refraction, and if there is use standard atmosphere instead
  if(sum(radioframe$thetap>0 & radioframe$theta<=radioframe$thetap,na.rm=T)>0){print(paste0('stuff aint right in: ',datereal,' using standard refraction instead'))
    beamhtall<-beamht_std(range=beamhtall$range, earthr=earthr, antht=antht)}
  
  return(beamhtall)
}
