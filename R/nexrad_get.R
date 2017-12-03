#' Automatically grabs variables related to specific radar calculations
#' 
#' This program will create variables in the global environment that are typically used for radar calculations. They include latitude, longitude, tower height, antenna height, and earth radius from the radar tower.
#' @param radar The 3 letter radar code
#' @param nexrad_n Location of the nexrad site table. Default is the location on the AP's server
nexrad_get<-function(radar,nexrad_n="V:/Documents/nexrad_site_list_with_utm.csv"){
  ####nexrad site parameters
  site <- read.csv(nexrad_n) 
  site_inf <<- subset(site, SITE==radar)
  baset <<-site_inf$BASE_HT_M
  twrht <<- site_inf$TOWER_HT_M
  antht <<- site_inf$ANT_HT_M
  lat <<- site_inf$LATITUDE_N
  long<<- site_inf$LONGITUDE_W
  earth <- (earthe*(1 - (0.0033493*(sin(lat*deg2rad))**2)))
  eartha <<- (earth+antht) 
  earthr <<- 4*eartha/3
}