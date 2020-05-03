# ======== Estadisticos de orden
if(1) {
  #======
  prob <- "Estadisticos de Orden - normal"
  cat(paste("\n",prob,":\n\n",sep=""))
  N <- 5000
  n <- 25
  k <- 5
  
  mu <- 10
  sigma <- 2
  
  xm <- rep(NA, N)
  xM <- rep(NA, N)
  xq <- rep(NA, N)
  
  X <- matrix(NA,nrow=N, ncol=n)
  for(i in seq(N)) {
    x <- rnorm(n, mean=mu, sd=sigma)
    xm[i] <- min(x)
    xM[i] <- max(x)
    xq[i] <- sort(x)[k]
    X[i,] <- x
  }
  xlim <- range(X)
  
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(1,1), mgp=c(1.5,.5,0), mar=c(4,3,2,1), oma=c(0,0,0,0), pty="m", las=1, 
      cex=1.00, cex.lab=0.9, cex.main=1.0, cex.axis=0.9)
  col.null <- "lightgreen"
  col.m <- "red"
  col.M <- "blue"
  col.k <- grey(.7)
  
  X <- rnorm(2*N, mean=mu, sd=sigma)
  tt0 <- hist(X, plot=FALSE)
  plot(tt0, xlim=xlim, axes=FALSE, xlab="", ylab="", main="", 
       col=col.null, border=col.null, density=20, angle=-45)
  tt <- hist(xm, plot=FALSE)
  plot(tt, add=TRUE, col=col.m, border=col.m, density=20, angle=30)
  tt <- hist(xM, plot=FALSE)
  plot(tt, add=TRUE, col=col.M, border=col.M, density=20, angle=-30)
  tt <- hist(xq, plot=FALSE)
  plot(tt, add=TRUE, col=col.k, density=25, angle=45)
  qlab <- paste(k,"-?simo",sep="")
  lab <- c("Estadístico", "original", "mínimo", "máximo", qlab)
  legend("topleft", col=c(0, col.null, col.m, col.M, col.k), lwd=4, legend=lab, bty="n")
  lab <- paste(N,"muestras de tamaño", n)
  title("Distribución normal", sub=lab, line=1)

  par(opar)
  rm(list=ls())
# ======
}
# =================================
