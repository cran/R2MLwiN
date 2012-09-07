caterpillar = function(y, x, qtlow, qtup, xlab="",ylab="", xlim=NULL,ylim=NULL,main=""){
#This function draws a caterpillar plot

plot(x,y,xlim=xlim,ylim=ylim,pch=15, xlab=xlab,ylab=ylab,main=main)
points(x,qtlow,pch=24,bg="grey")
points(x,qtup,pch=25,bg="grey")
for(i in 1:length(x)) {lines(rep(x[i],2),c(qtlow[i],qtup[i]))}

}
