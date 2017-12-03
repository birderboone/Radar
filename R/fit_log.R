

fit_log<-function(top, bottom, data, parameter='four'){
  #data<-my.data
  n_spline<-spline(data$ref ~ data$d_time, n=100)
  if(any(n_spline$y<=0)){n_spline$y<-n_spline$y + abs(min(n_spline$y)) +.5}
  bottom_p<-which.min(abs(n_spline$x - bottom))
  top_p<-which.min(abs(n_spline$x - top))
  new_curve<-c(n_spline$y[bottom_p] + sample(x=(-100:100),size=(bottom_p-1))/750, n_spline$y[bottom_p:top_p], n_spline$y[top_p] + sample(x=(-100:100),size=(length(n_spline$y)-top_p))/750 )    
  ###this requires a top sunangle, bottom sunangle, and a spline function
  #plot(new ~ n_spline$x)
  groupd<-data.frame(ref=new_curve, sunangle=n_spline$x)
  ##############################################
  #groupd<-groupedData(ref~sunangle|date, data=my.data)
  groupd<-groupd[order(groupd$sunangle),]
  # ##########trouble shooting options#########
  # groupd<-groupd[0:(nrow(groupd)-7),]   ###for cropping data
  # groupd<-groupd[(-10),]                ###too delete a point
  # groupd$ref[6]<-(groupd$ref[6]+(1.1)) ###to move a point
  #plot(groupd$sunangle, groupd$ref)    ### to test if the fixes were appropriate
  ###########################################
  
  ###try to fit it
  if(parameter=='five') {model1<-nls(ref~(k /(( 1 + (exp ((-b) * (sunangle - i ) ) ) ) )^(1/v)+a),start=list(k=max(groupd$ref)-(min(groupd$ref)),i=((top+bottom)/2),a=(min(groupd$ref)),b=1,v=1),lower=c(k=(max(groupd$ref)-(min(groupd$ref)))/2,i=21,a=min(groupd$ref)/2,b=.00001,v=.1),upper=c(k=(max(groupd$ref)*1.5),i=top,a=min(groupd$ref)*1.5,b=1000,v=1.9),nls.control(maxiter=100),data=groupd,algorithm='port')
  

  
  
  
  pars<-coef(model1)
  curved<-data.frame(k=pars[1],i=pars[2],a=pars[3],b=pars[4], v=pars[5])#m=pars[3]
  plot(groupd$sunangle, groupd$ref,xlim=c(min(groupd$sunangle,na.rm=T)-.25,max(groupd$sunangle,na.rm=T)),ylim=c(min(groupd$ref)-.25,max(groupd$ref)+.5))
  title(main=paste0(names(here)[[pp]],'_(',pp,')'))
  legend('right', paste0(names(pars),' = ',signif(pars,4)))
  curve((pars[1] /(( 1 + (exp ((-pars[4]) * (x - pars[2] ) ) ) ) ^(1/pars[5]) )+pars[3]),add=T)
  
  
  
  #plot(new ~ n_spline$x)
  groupd<-data.frame(ref=new_curve, sunangle=n_spline$x)
  
  plot(n_spline, xaxt='n');title(main=paste0(unique(data$date)[1]));axis(1, at= (1:3000)/100, labels=F);axis(1, at= (1:3000)/25, las=2) ;abline(v=n_spline$x[which.min(abs(data$sunangle-90))],col='red') ;abline(v=pars['i'],col='black')
  curve((pars[1] /(( 1 + (exp ((-pars[4]) * (x - pars[2] ) ) ) ) ^(1/pars[5]))+pars[3]),add=T)
  legend('bottomright', c('sunset','inflection',paste0(names(pars),' = ',signif(pars,4))),col=c('red','black'),lty=c(1,1,0,0,0,0))
  pars<<-list(k=pars[1],i=pars[2],a=pars[3],b=pars[4],v=pars[5])
}

###############
###four parameter
if(parameter=='four') {model1<-nls(ref~(k /(( 1 + (exp ((-b) * (sunangle - i ) ) ) ) )+a),start=list(k=max(groupd$ref)-(min(groupd$ref)),i=((top+bottom)/2),a=(min(groupd$ref)),b=1),lower=c(k=(max(groupd$ref)-(min(groupd$ref)))/2,i=21,a=min(groupd$ref)/2,b=.00001),upper=c(k=(max(groupd$ref)*1.5),i=top,a=min(groupd$ref)*1.5,b=1000),nls.control(maxiter=100),data=groupd,algorithm='port')

pars<-coef(model1)
curved<-data.frame(k=pars[1],i=pars[2],a=pars[3],b=pars[4])#m=pars[3]
plot(groupd$sunangle, groupd$ref,xlim=c(min(groupd$sunangle,na.rm=T)-.25,max(groupd$sunangle,na.rm=T)),ylim=c(min(groupd$ref)-.25,max(groupd$ref)+.5))
title(main=paste0(names(here)[[pp]],'_(',pp,')'))
legend('right', paste0(names(pars),' = ',signif(pars,4)))
curve((pars[1] /(( 1 + (exp ((-pars[4]) * (x - pars[2] ) ) ) ))+pars[3]),add=T)



#plot(new ~ n_spline$x)
groupd<-data.frame(ref=new_curve, sunangle=n_spline$x)

plot(n_spline, xaxt='n');title(main=paste0(unique(data$date)[1]));axis(1, at= (1:3000)/100, labels=F);axis(1, at= (1:3000)/25, las=2) ;abline(v=n_spline$x[which.min(abs(data$sunangle-90))],col='red') ;abline(v=pars['i'],col='black')
curve((pars[1] /(( 1 + (exp ((-pars[4]) * (x - pars[2] ) ) ) ) )+pars[3]),add=T)
legend('bottomright', c('sunset','inflection',paste0(names(pars),' = ',signif(pars,4))),col=c('red','black'),lty=c(1,1,0,0,0,0))
pars<<-list(k=pars[1],i=pars[2],a=pars[3],b=pars[4])
}
}