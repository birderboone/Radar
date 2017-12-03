#' Downloads radiosondes from the university of Wyoming website for a radar in .html format
#' 
#' This program uses site ID's taken from the University of Wyoming that are stored in the nexradsite table. You may need to update that file to download radiosondes. Output is in .html.
#' @param dates in as.POSIXlt format
#' @param output Location to save files
#' @param radar The 3 letter radar code
#' @param years The years you want to download. Default is all years in the dates list.
#' @param nexrad_n Location of the nexrad site table. Default is the location on the AP's server
radio_get<-function(dates, output, radar, nexrad_n,years=unique(nights2$year) +1900,overwrite=F){
  #get nexrad info from table
  nexrad_get(radar=radar,nexrad_n=nexrad_n)
  
  
  radar_id <-paste0(site_inf$SONDE)  ##station identification (from WYoming website)
  ######turn dates into date format and calculate the correct day + 1 for the radiosonde download
  nights2<-dates  ###this needs to be correct big Y for long years little y for 2 day year
  nights2<-nights2[(nights2$year+1900) %in% as.character(years) ]
  radiodate<-nights2+86400
  
  ##########################################################################################
  ###                                     DO NOT TOUCH                                    ##
  url<-c("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=????&MONTH=??&FROM=??00&TO=??00&STNM=?????")
  ##########################################################################################
  ##########################################################################################
  # download all files
  all_radio<-list.files(output)
  for (i in 1:length(radiodate)){
    radio_name<-paste0('K',radar,'_',substr(radiodate[i],1,4),substr(radiodate[i],6,7), substr(radiodate[i],9,10),'.htm')
    if(radio_name%in%all_radio & overwrite==F){next}
    substring(url,78)<-substr(radiodate[i],1,4)
    substring(url,89)<-substr(radiodate[i],6,7)
    substring(url,97)<-substr(radiodate[i],9,10)
    substring(url,105)<-substr(radiodate[i],9,10)
    substring(url,115)<-radar_id
    #print(url)
    (download.file(url,destfile=paste0(output,'/K',radar,'_',substr(radiodate[i],1,4),substr(radiodate[i],6,7), substr(radiodate[i],9,10),'.htm')))
    
  }}
#########################################################################################