exodus_pack<-function(data, out_exodus,nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv", radar){
  require(RAtmosphere)
  nexrad1<-read.csv(nexrad_n)
  nexrad<-subset(nexrad1,SITE==radar)
  lat=nexrad$LAT
  long=nexrad$LONG
#answer2<-data.frame(date=colnames(data))
answer2<-data.frame(t(data))
answer2$date<-colnames(data)
#(par[1] /(( 1 + (exp ((-par[4]) * (seq(1,50,.1) - par[2] ) ) ) ) )+par[3])
#answer2$sunangle<-as.numeric(t(data[2,]))
#answer2$strength<-as.numeric(t(data[1,]))
#answer2$roundangle<-round(answer2$sunangle*2)/2
answer2<-na.omit(answer2)   ##take out NAs
answer2$start<-NA
answer2$end<-NA
answer2$sunset<-NA
for(i in 1:nrow(answer2)){
  dd<-(answer2$k[i] /(( 1 + (exp ((-answer2$b[i]) * (seq(21,26,.01) - answer2$i[i] ) ) ) ) )+answer2$a[i])
  #answer2[1,]
  #plot((par[1] /(( 1 + (exp ((-par[4]) * (seq(21,26,.01) - par[2] ) ) ) ) )+par[3]) ~ seq(21,26,.01))
  answer2$start[i]<-seq(21,26,.01)[which(dd>answer2$k[i]*.05 + answer2$a[i])[1]]
  answer2$end[i]<-seq(21,26,.01)[which(dd>(answer2$k[i] + answer2$a[i])*.95)[1]]
  
  tt1<-as.POSIXlt((as.POSIXlt(paste0(answer2$date[i],' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:14400)[which.min(abs(SZA(as.POSIXlt(paste0(answer2$date[i],' 21:00'),format='%Y-%m-%d %H:%M',tz='UTC') + 1:14400,lat,long)-90))],format='%Y-%m-%d %H:%M:%S')
  answer2$sunset[i]<-to_dhour(tt1)
  ###################################
  total <- nrow(answer2)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  setTxtProgressBar(pb, i)
  ###################################
}  
answer2$sunset[answer2$sunset<12]<-answer2$sunset[answer2$sunset<12] +24
answer2$duration<-answer2$end-answer2$start
tt<-answer2$i-4
tt2<-as.POSIXct(paste0(answer2$date,' ',floor(tt) ,':', floor((tt-floor(tt)) *60),':' ,round(((tt-floor(tt)) *60-floor((tt-floor(tt)) *60)) *60)), format='%Y-%m-%d %H:%M:%S',tz='UTC') + 4*60*60
answer2$sunangle<-SZA(tt2,lat,long)
answer2$julian<-as.POSIXlt(paste0(answer2$date,' 23:00'),format='%Y-%m-%d %H:%M')$yday
#answer2
#plot((start-sunset) ~ julian, data=answer2)
#plot(sunangle ~ julian, data=answer2)
#plot(duration ~ julian, data=answer2)
#############################
#paste0('T:/exodusangles/',radar,'_exodusangle_time_',folder,'.csv')
write.csv(answer2,out_exodus,row.names=F,quote=F)
print('successfully wrote out exodus angle table and wdssii angle table')}