`Cambridgecolourdemo` <-
function(){
plot(c(0,3),c(0,2),type="n",xlab="",ylab="",axes=F)
box()
rect(0,0,1,1,col=Camcol("LightBlue"),border=F)
rect(0,1,1,2,col=Camcol("DeepBlue"),border=F)
rect(1,0,2,1,col=Camcol("Green"),border=F)
rect(1,1,2,2,col=Camcol("Burgundy"),border=F)
rect(2,0,3,1,col=Camcol("Red"),border=F)
rect(2,1,3,2,col=Camcol("Yellow"),border=F)
}

