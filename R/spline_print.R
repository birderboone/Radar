#' Graphs spline fitted exodus curves, with sunset included
#' 
#' This function is specific to the curve-fitting process and likely not of important use outside of this program
#' @param my.data  a data.frame with two columns: d_time = decimal time and ref = reflectivity.
spline_print<-function(my.data){
  #plot(my.data$d_time, my.data$ref)  ###intially plot uncropped
  n_spline<-spline(my.data$ref ~ my.data$d_time,n=200)
  plot(n_spline, xaxt='n', ylab='reflectivity', xlab='decimal hour');title(main=paste0(unique(my.data$date)[1]));axis(1, at= (1:800)/100, labels=F);axis(1, at= (1:800)/25, las=2) ;abline(v=n_spline$x[which.min(abs(spline(my.data$ref ~ my.data$sunangle)$x-90))],col='red')
  #print(data.frame(x=n_spline$x, y=n_spline$y))
  points(my.data$ref ~ my.data$d_time, col='red')
  warning(paste0('red = sunset, black circle = interpolated reflectivity, red circle = actual reflectivity'),call.=F)
}

