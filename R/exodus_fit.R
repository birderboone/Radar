exodus_fit<-function(nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv" ,out_netcdf, radar){
  require(RAtmosphere)
  require(nlme)
  nexrad1<-read.csv(nexrad_n)
  nexrad<-subset(nexrad1,SITE==radar)
  lat=nexrad$LAT
  long=nexrad$LONG
  final<-read.csv(out_netcdf)
  
  times<-as.POSIXlt(final$times,format='%Y-%m-%d %H:%M')
  final$d_time<-to_dhour(times)
  final$d_time[final$d_time<12]<-final$d_time[final$d_time<12] + 24
  
  final$ref<-final$ref+abs(min(final$ref))
  here<-by(final,list(final$date),data.frame)
  
  answerframe<-data.frame(matrix(nrow=4,ncol=dim(here)))
  
  row.names(answerframe)<-c('k','i','a','b')
  colnames(answerframe)<-names(here)
  final<-final[order(final$times),]
  ################################################
  ###TAKE A BREAK SON
  ################################################
  ##changes plot window
  par( mfrow = c( 4, 4 ) )
  par(mar=c(1.75,1.75,1.75,1.75))
  ################################################
  for(pp in 1:dim(here)){  
    #pp<-1
    my.data<-here[[pp]]
    my.data<-my.data[order(my.data$d_time),]
    
    groupd<-groupedData(ref~d_time|date, data=my.data)
    groupd<-groupd[order(groupd$sunangle),]
    groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]<-groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]+groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]*.01
    
    if(abs(which.max(my.data$ref)-nrow(my.data))<=1){groupd<-groupd[(which.min(my.data$ref)-1):(which.max(my.data$ref)),]}else{
      groupd<-groupd[(which.min(my.data$ref)-1):(which.max(my.data$ref)+2),]   ###for cropping data 
    }
    if(dim(groupd)[1]<5) {print(paste0(names(here)[[pp]],'_(',pp,') has too few observations')); next}
    groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]<-groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]+groupd$ref[(which(abs(groupd$ref[1:(nrow(groupd)-1)]-groupd$ref[2:nrow(groupd)])<.1)+1)]*.01
    
    radio<-try(    model1<-nls(ref~(k /(( 1 + (exp ((-b) * (d_time - i ) ) ) ) )+a),start=list(k=max(my.data$ref)-(min(my.data$ref)+.25),i=24,a=1,b=1),lower=c(k=.5,i=21,a=.0001,b=.01),upper=c(k=(max(my.data$ref)+.5),i=25,a=20,b=10),nls.control(maxiter=100),data=groupd,algorithm='port'))
    
    if ('try-error' %in% class(radio)) {next} 
    model1<-nls(ref~(k /(( 1 + (exp ((-b) * (d_time - i ) ) ) ) )+a),start=list(k=max(my.data$ref)-(min(my.data$ref)+.25),i=24,a=1,b=1),lower=c(k=.5,i=21,a=.0001,b=.01),upper=c(k=(max(my.data$ref)+.5),i=25,a=20,b=10),nls.control(maxiter=100),data=groupd,algorithm='port')
    
    par<-coef(model1)
    plot(groupd$d_time, groupd$ref,xlim=c(min(groupd$d_time,na.rm=T)-.1,max(groupd$d_time)),ylim=c((min(my.data$ref)-1),max(my.data$ref)+1))
    title(main=paste0(names(here)[[pp]],'_(',pp,')'))
    k<-par[1]
    i<-par[2]
    
    a<-par[3]
    b<-par[4]
    curve((k /(( 1 + (exp ((-b) * (x - i ) ) ) )  )+a),add=T)
    
    i
    answerframe[,pp]<-c(par[1],par[2],par[3],par[4])
    names(here)[[pp]]
    
  }
  
  bad<-numeric()
  if(sum(is.na(answerframe[1,]))>0){print(paste0(colnames(answerframe)[is.na(answerframe[1,])],'_(',which(is.na(answerframe[1,])), ') could not fit'))
    bad<-paste0(colnames(answerframe)[is.na(answerframe[1,])],'_(',which(is.na(answerframe[1,])),')')}
  bad<<-bad
  answerframe<<-answerframe
  here<<-here
  #bad
  ####writes out a file called 'bad' which can be referred to later
  
  ##returns plot window to default values
  par( mfrow = c( 1, 1 ) )
  par ( mar= c(5, 4, 4, 2) )
}