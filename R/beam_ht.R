# #' Calculates the height of the radar beam using standard refraction
# #' 
# #' Output is a list containing the bottom, middle, and top of the beam.  
# #' Use beamht_radio if you want to calculate beam heights using radiosonde
# #' @param range numeric range of each pulse volume 
# #' @param groundht the ground height above sea level at each pulse volume
# #' @param elev the elevation of  the radar beam. Can be a scalar or vector
# #' @param earthr the earths radius in meters at the radar location, typically created by nexrad_get
# #' @param antht the height of the antenna at the radar location, typically created by nexrad_get
# 
# 
# beam_ht<-function(range,groundht,elev=.5, earthr=earthr, antht=antht){
#   
#   mbeam <- sqrt(earthr^2+range^2+(2*range*earthr*sin(deg2rad*elev)))-(earthr)+antht-groundht
#  bbeam <- mbeam-range*halfbeam*(cos(deg2rad*elev))
#   tbeam <- mbeam+(range*halfbeam*(cos(deg2rad*elev)))
#   list(mbeam=mbeam, bbeam=bbeam, tbeam=tbeam)
#   
#   
# }