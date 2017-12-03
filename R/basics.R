#' Calculates the standard error of the mean
#' @param x  vector of values
#' @param na.rm  removes NA values automatically. Default=T
sem<-function(x,na.rm=T){sd(x,na.rm=na.rm)/sqrt(length(x))}

#' Calculates the geometric mean
#' @param x  vector of values
#' @param na.rm removes NA values automatically, Default=T
geom<-function(x,na.rm=T){exp(mean(log(x+1),na.rm=na.rm))-1}

#' Time to decimal hour converter  
#' 
#' Returns a time in decimal hours. 
#' @param times  a vector of times class 'time'
#' @param mid  whether you want to convert to 24+ time for analysis across midnight. this is off by default
to_dhour<-function(times, mid=F) {dtime<- times$hour + times$min/60 + times$sec/60/60
if(mid==T & dtime<12){dtime<-dtime+24}
dtime
}   ##must be as.POSIXlt format

#' Rounds to any decimal or whole value
#' 
#' @param x  vector of values
#' @param dec the number/decimal you want to convert to. 
mround <- function(x,dec){ dec*round(x/dec) }


#' Converts decimal time to an as.POSIX time.
#' 
#' The output has a generic tz of UTC.
#' @param date  the date in the format : yyyy-mm-dd
#' @param d_time  decimal time as a numeric. Can be in 24+ time however it will not change the date accordingly

dtime_2_clock<-function(date, d_time){
  dt<-as.POSIXct(date, format='%Y-%m-%d')
  dt[d_time>=24.0]<-dt[d_time>=24.0]+60*60*24
  dt<-substr(dt,1,10)
  d_time[d_time>=24.0]<-d_time[d_time>=24.0]-24
  tt<-d_time
  tt2<-paste0(floor(tt) ,':', floor((tt-floor(tt)) *60),':' ,round(((tt-floor(tt)) *60-floor((tt-floor(tt)) *60)) *60))
  as.POSIXlt(paste0(dt,' ',tt2), format='%Y-%m-%d %H:%M:%S', tz='UTC')
}


#' Incomparables: A function that compares one value against a vector of options and returns the closest value to it. 
#' 
#' The function is primarily for situations where the numbers are not exact matches. Does not distinguish between positive or negative differences.
#' @param x  vector of values to match against
#' @param comp  the singular number to match
incomp<-function(x, comp){
  try(if(length(comp)!=1)stop('number is not singular',call.=F))
  
  x[which.min(abs(x-comp))]}


#' Converts dbz to z in reflectivity.

#' @param x  vector in units of dbz
dbz2z<-function(x){
  10^(x/10)
}


#'  Converts z to dbz in reflectivity
#'
#' @param x  vector in units of z
z2dbz<-function(x){
  10*log10(x)
}

#' This gives you a unit of time in class 'time'
#' 
#' This function is useful when you need to find the difference in time of two objects but require a reference number in class of time to compare against.
#' 
#' @param number  a numeric of how many of time units you want
#' @param unit the unit of time accepts 'sec', 'min', 'hour', or 'day'
time_diff<-function(number, unit){
  if(!(unit%in%c('sec','min','hour','day'))){print('invald unit entry')}
  if(unit=='sec'){time<-(Sys.time() + number -Sys.time())} 
  if(unit=='min'){time<-Sys.time() + number*60 -Sys.time()}
  if(unit=='hour'){time<-Sys.time() + number*60*60 -Sys.time()}
  if(unit=='day'){time<-Sys.time() + number*60*60*24 -Sys.time()}
  time
}

#' Adds a clock to a for loop
#' @param x  the vector used in the for loop
#' @param var  what is the variable name in the for loop
clockz<-function(x, var){
  total <- length(x)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  setTxtProgressBar(pb, which(var==x))
}
