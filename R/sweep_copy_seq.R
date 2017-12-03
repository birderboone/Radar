#' Transfers raw radar files from a raw folder to an output folder based on a list of specified angles
#' 
#' This program is specifically for grabbing sweeps from a list of specific angles. It chooses the closest file within 1 sun elevation of your specified angle. File folders for raw folders must be split into seperate year folders. The program is expecting the file system to be: rawfolder/2010 ,rawfolder/2011, rawfolder/2012. If you have these seperated differently, you need to change the file system so that the terminal folder is the year. If necessary you can input a complex path in the output field. For example: if rawfolder = example/project, and the years are in example/project/year, then it will work. A caution when calculating on a large server, this program initially gathers all file information before copying, therefore it may take a long time to initiate on a cluttered server
#' @param birdnight nights you want to select sweeps. Date needs to be in the format: 'mm/dd/yyyy'
#' @param rawfolder base folder where raw files are located. Terminal folder must be years
#' @param output location of the selected files
#' @param radar 3 letter radar code
#' @param seq_angle a vector of specific angles you want chosen
#' @param years a vector of the unique years you want to select from. Default is all years in the birdnight vector
#' @param morning T/F whether you want to grab morning times. This reverses the sun angles and only grabs files from the morning
#' @param folders Whether you want each night to be seperated into seperate folders. This is required if you're using these files to calculate exodus curves.
#' @param nexrad_n  location of the nexrad site table. Default is the location on the AP's server
#########################################startwork###########################################
sweep_copy_seq<-function(birdnight, rawfolder, output, radar,seq_angle,morning=F,years=unique(nights2$year) +1900,folders=T, nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv",tol=2){
  #############################################################################################
  #############################################################################################
  require(RAtmosphere)
  #############################################################################################
  #get nexrad info from table
  nexrad_get(radar,nexrad_n=nexrad_n)
  #nexrad <- read.csv(nexrad_n)
  #nexrad <- subset(nexrad,SITE==substr(radar,2,5))
  
  radar_id <-site_inf$SONDE  ##station identification (from WYoming website)
  ######turn dates into date format and calculate the correct day + 1 for the radiosonde download
  
  nights2<-as.POSIXlt(birdnight,format='%m/%d/%Y')   ###this needs to be correct big Y for long years little y for 2 day year
  if(morning==T){nights2<-as.POSIXlt(as.POSIXct(nights2,format='%Y-%m-%d') + 60*60*24)}
  #if( missing('years')){years<-unique(nights2$year) +1900}
  nights2<-nights2[(nights2$year+1900) %in% as.character(years) ]
  
  
  dnight<-as.POSIXct(substr(nights2,1,10),format='%Y-%m-%d')
  u_year<-unique(nights2$year+1900)
  
  print('attempting to read raw files, standby...')
  year_list<-list()
  year_list<-lapply(u_year,function(x)na.omit(list.files(paste0(rawfolder,'/',x),pattern='.gz')))
  year_time<-lapply(year_list,function(x) as.POSIXlt(substring(x,5,19),format='%Y%m%d_%H%M%S',tz='UTC'))
  if(morning==F){year_d<-lapply(year_time,function(x)substr(x-60*60*12,1,10))}
  if(morning==T){year_d<-lapply(year_time,function(x)substr(x,1,10))}
  #print(year_time)
  year_angle<-lapply(year_time,function(x){SZA_pn(x,Lat=lat,Lon=long)})
  badnight<-numeric()
  print('starting to copy files')
  for(i in 1:length(nights2)){
    #i<-1
    fold<-substr(nights2[i],1,10)
    year<-nights2[i]$year+1900 
    fileangle<-year_angle[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    files<-year_list[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    filed<-year_d[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    filetimes<-year_time[[which(u_year==as.numeric(substr(dnight[i],1,4)))]]
    timez<-nights2[i]
    lapply(seq_angle, function(x){x
      
      nightfile<-files[dnight[i]==filed]
      nightangle<-fileangle[dnight[i]==filed]
      final_file<-nightfile[nightangle==incomp(nightangle, x) & abs(incomp(nightangle, x) - x)<tol]
      if(folders==F){file.copy(paste0(rawfolder,'/',year,'/',final_file),paste0(output))}
      
      if(folders==T){ dir.create(paste0(output,'/',fold),showWarnings=FALSE)
        file.copy(paste0(rawfolder,'/',year,'/',final_file),paste0(output,'/',substr(nights2[i],1,10)))
      }
    })
    #if(length(files[dnight[i]==filed & (fileangle>eangle & fileangle <langle)])<=5){badnight<-append(badnight,as.character(dnight[i]))}
    
  }
  print('Finished copying.')
  #print(badnight)   ###these are the nights that had less than 6 sweeps chosen
  
}