#' Calculates a positive/negative sunangle depending on time of day
#' 
#' This program converts the standard SZA function in RAtmosphere to one that calculates positive and negative sunangles. Positive sunangles are ones from noon - midnight, and negative angles are ones between midnight - noon
#' @param timein as.POSIX format of time
#' @param Lat latitude
#' @param Lon longitude
SZA_pn<-function (timein = Sys.time(), Lat = 50.910335, Lon = 11.56874) 
{
  #require(RAtmosphere)
  sza <- vector("numeric", length = length(timein))
  sza_f <- vector("numeric", length = length(timein))
  for (i in 1:length(timein)) {
    time <- as.POSIXlt(timein[i], tz = "GMT")
    d2r = pi/180
    r2d = 1/d2r
    d <- 23.45 * d2r * sin(d2r * 360 * (284 + time$yday)/365)
    if (time$yday <= 106) {
      E_qt <- -14.2 * sin(pi * (time$yday + 7)/111)
    }else {
      if (time$yday <= 166) {
        E_qt <- 4 * sin(pi * (time$yday - 106)/59)
      }else {
        if (time$yday <= 246) {
          E_qt <- -6.5 * sin(pi * (time$yday - 166)/80)
        }else {
          E_qt <- 16.4 * sin(pi * (time$yday - 247)/113)
        }
      }
    }
    T <- time$hour + time$min/60 + time$sec/3600
    Longitude <- Lon
    T_solar <- T + Longitude/15 + E_qt/60
    w <- pi * (12 - T_solar)/12
    l <- Lat * d2r
    sza[i] <- 90 - asin(sin(l) * sin(d) + cos(l) * cos(d) * 
                          cos(w)) * r2d
    
    time <- as.POSIXlt(timein[i] + 10, tz = "GMT")
    d2r = pi/180
    r2d = 1/d2r
    d <- 23.45 * d2r * sin(d2r * 360 * (284 + time$yday)/365)
    if (time$yday <= 106) {
      E_qt <- -14.2 * sin(pi * (time$yday + 7)/111)
    }else {
      if (time$yday <= 166) {
        E_qt <- 4 * sin(pi * (time$yday - 106)/59)
      }else {
        if (time$yday <= 246) {
          E_qt <- -6.5 * sin(pi * (time$yday - 166)/80)
        }else {
          E_qt <- 16.4 * sin(pi * (time$yday - 247)/113)
        }
      }
    }
    T <- time$hour + time$min/60 + time$sec/3600
    Longitude <- Lon
    T_solar <- T + Longitude/15 + E_qt/60
    w <- pi * (12 - T_solar)/12
    l <- Lat * d2r
    sza_f[i] <- 90 - asin(sin(l) * sin(d) + cos(l) * cos(d) * 
                          cos(w)) * r2d
    
    
    
   if((sza[i] - sza_f[i])>0) {sza[i] <- (-sza[i])}
    if((sza[i] - sza_f[i])<0) {sza[i] <- (sza[i])}
  }
  return(sza)
  
}