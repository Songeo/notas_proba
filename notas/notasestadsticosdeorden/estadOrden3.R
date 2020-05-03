#======== Estadisticos de orden
if(0) {
#======
prob <- "Estadisticos de Orden"
cat(paste("\n",prob,":\n"))

n <- 9
#x <- runif(n)
x <- rgamma(n,2,1)
y <- sort(x)
i <- order(x)
j <- match(x,sort(x))
print(round(data.frame(x,j, y,i),3))
xmax <- round(max(x),3)
xmin <- round(min(x),3)
rango <- xmax-xmin
cat("Rango de la muestra =",xmax,"-",xmin,"=",rango,"\n")
rm(list=ls())
#======
}
#=================================

#======== Estadisticos de orden
if(1) {
#======
prob <- "Estadisticos de Orden"
cat(paste("\n",prob,":\n"))

N <- 12
n <- 8
k <- 3

col <- seq(2,n+1)
x <- matrix(NA,nrow=N,ncol=n,dimnames=list(seq(N),paste("x",seq(n),sep="")))
xmin <- rep(NA,N)
xmax <- rep(NA,N)
idxm <- rep(NA,N)
idxM <- rep(NA,N)
for(i in seq(N)) {
    x[i,] <- runif(n)
    xmin[i] <- min(x[i,])
    idxm[i] <- which.min(x[i,])
    xmax[i] <- max(x[i,])
    idxM[i] <- which.max(x[i,])
}
lab <- paste("Máximos y mínimos de",N,"muestras de tamano",n,
    "\n de una poblacion (distribucion) uniforme (0,1)")
cat(paste(lab,"\n"))
print(t(round(x <- cbind(x,xmin=xmin,xmax=xmax,rango=xmax-xmin),3)))

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,1),mgp=c(1.5,.5,0),mar=c(3,3,3,4),oma=0*c(1,1,1,1),pty="m")
ylim <- c(0,1)  #range(xmin,xmax)
plot(0,0,xlim=c(1,N),ylim=ylim,type="n",
    xlab="ensayo (muestra)",ylab="realizaciones",
    main=lab,cex.main=0.9)
rect(k-0.5,-0.03,k+0.5,1.03,col=grey(.98),border=1)
for(i in seq(N)) {
    points(rep(i,n),x[i,seq(n)],pch=20,col=col)
    points(rep(i,2),c(xmin[i],xmax[i]),pch=c(25,24),col=1,cex=1.25)
}
axis(4,labels=FALSE)
points(jitter(rep(N+1,N)),xmin,col=col[idxm],pch=25,xpd=NA,)
#points(rep(N+1/2,N),xmin,col=col[idxm],pch=21,xpd=NA,)
points(jitter(rep(N+1,N)),xmax,col=col[idxM],pch=24,xpd=NA)
#points(rep(N+2/3,N),xmax,col=col[idxM],pch=22,xpd=NA)
#mtext(side=4,"texto",line=-3,xpd=NA)
par(opar)

#require(misc)
#savePlots("estadOrden",8,6)

rm(list=ls())
#======
}
#=================================
