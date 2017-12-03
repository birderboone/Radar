#'Resamples azimuths
#'
#'@param data A dataframe needing to be resampled
#'@param old_azim Vector of old_azimuths that matches the data
#'@param new_azim A sequence of azimuths to resample data to

re_azim<-function(data,old_azim, new_azim=seq(0,359.5,.5),r_bind=T,tol=1){
  tt<-lapply(new_azim, function(z){
    az_a<-incomp(unique(old_azim),z)
    if(abs(az_a-z)<=tol){
    dd<-subset(data, old_azim==az_a)
    dd$azimuth<-z
    return(dd)}
  })
 if(r_bind==T){df<-do.call(rbind, tt)}
  if(r_bind==F){df<-as.data.frame(tt)}
  return(df)
}

re_range<-function(data,old_range, new_range=seq(250,100000,250),r_bind=T,tol=1000){
  tt<-lapply(new_range, function(z){
    az_a<-incomp(unique(old_range),z)
    if(abs(az_a-z)<=tol){
      dd<-subset(data, old_range==az_a)
      dd$range<-z
      return(dd)}
  })
  if(r_bind==T){df<-do.call(rbind, tt)}
  if(r_bind==F){df<-as.data.frame(tt)}
  return(df)
}