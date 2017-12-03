#########################################startwork###########################################
exodus_copy_angle<-function(birdnight, sunangle,rawfolder, range, output, radar, years,nexrad_n){
  #############################################################################################
  require(RAtmosphere)
  #############################################################################################
  #get nexrad info from table
  nexrad_get(radar=substr(radar,2,5))
  nexrad <- read.csv(nexrad_n)
  nexrad <- subset(nexrad,SITE==substr(radar,2,5))
#   lat <- nexrad$LAT
#   long <- nexrad$LONG
  radar_id <-paste0(nexrad$SONDE[nexrad$SITE==substr(radar,2,4)])  ##station identification (from WYoming website)
  ######turn dates into date format and calculate the correct day + 1 for the radiosonde download
  nights1<-as.POSIXlt(birdnight,format='%Y-%m-%d')   ###this needs to be correct big Y for long years little y for 2 day year
  if( missing('years')){years<-unique(nights1$year) +1900}
  nights2<-nights1[(nights1$year+1900) %in% years ]
  sunangle<-sunangle[(nights1$year+1900) %in% years ]
  
  dnight<-as.POSIXct(substr(nights2,1,10),format='%Y-%m-%d')
  u_year<-unique(nights2$year+1900)
  year_list<-list()
  year_list<-lapply(u_year,function(x)na.omit(list.files(paste0(rawfolder,'/',x))))
  year_time<-lapply(year_list,function(x) as.POSIXlt(substring(x,5,19),format='%Y%m%d_%H%M%S'))
  year_d<-lapply(year_time,function(x)substr(x-43200,1,10))
  year_angle<-lapply(year_time,function(x){SZA(x,lat,long)})
  badnight<-numeric()
  for(i in 1:length(nights2)){
    #i<-1
    sangle<-sunangle[i] - range/5
    eangle<- sunangle[i] + range/5
    fold<-substr(nights2[i],1,10)
    year<-nights2[i]$year+1900 
    fileangle<-year_angle[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    files<-year_list[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    filed<-year_d[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    filetimes<-year_time[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    timez<-nights2[i]
    dir.create(paste0(output,'/',fold),showWarnings=FALSE)
    file.copy(paste0(rawfolder,'/',year,'/',files[dnight[i]==filed & (fileangle>sangle & fileangle <eangle) & filetimes$hour%in%c(0:3,20:24)]),paste0(output,'/',substr(nights2[i],1,10)))
    if(length(files[dnight[i]==filed & (fileangle>sangle & fileangle <eangle) & filetimes$hour%in%c(0:3,20:24)])<=5){badnightbbadnight<-append(badnight,as.character(dnight[i]))}
    
  }
  print('Finished copying. Nights that have less than 6 sweeps:')
  print(badnight)   ###these are the nights that had less than 6 sweeps chosen

}