`Camcol` <-
function(col="LightBlue"){

col<-pmatch(col,c("LightBlue","DeepBlue","Green","Burgundy","Red","Yellow"))

if(col==1){
return(rgb(153,204,255,maxColorValue=255))
}
if(col==2){
return(rgb(0,51,102,maxColorValue=255))
}
if(col==3){
return(rgb(0,51,0,maxColorValue=255))
}
if(col==4){
return(rgb(51,0,0,maxColorValue=255))
}
if(col==5){
return(rgb(255,51,0,maxColorValue=255))
}
if(col==6){
return(rgb(255,204,0,maxColorValue=255))
}
}

