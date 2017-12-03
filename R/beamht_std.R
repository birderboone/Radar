#' Calculates the height of the radar beam using standard refraction
#' 
#' Output is a list containing the bottom, middle, and top of the beam.  
#' Use beamht_radio if you want to calculate beam heights using radiosonde
#' @param range numeric range of each pulse volume 
#' @param groundht the ground height above sea level at each pulse volume
#' @param elev the elevation of  the radar beam. Can be a scalar or vector
#' @param radar  the 3 letter radar code
#' @param nexrad_n  location of the nexrad site table. Default is the location on the AP's server
beamht_std<-function(range,groundht=0,elev=.5, radar=radar,nexrad_n='V:/Documents/nexrad_site_list_with_utm.csv'){
  nexrad_get(radar=radar,nexrad_n=nexrad_n)
  mbeam <- sqrt(earthr^2+range^2+(2*range*earthr*sin(deg2rad*elev)))-(earthr)+antht-groundht
  bbeam <- mbeam-range*halfbeam*(cos(deg2rad*elev))
  tbeam <- mbeam+(range*halfbeam*(cos(deg2rad*elev)))
  final<-list(mbeam=mbeam, bbeam=bbeam, tbeam=tbeam)
  return(final)
  
}