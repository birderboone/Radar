dumb<-function(x,y){
  here<-unlist(x)
  where<-unlist(y)
  lm1<-ros(here[!is.na(here)],where[!is.na(here)])
  myros<-lm1$modeled
  #c(predict(myros,0), mean(myros), median(myros), sd(myros))}, here1, where1)
  c(predict(lm1,0),mean(myros),median(myros),sd(myros,na.rm=T)/mean(myros,na.rm=T),sum(as.numeric(where),na.rm=T)/length(where[!is.na(here)]),length(where[!(is.na(here))]))}