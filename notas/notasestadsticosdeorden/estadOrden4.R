#======== Estadisticos de orden
if(1) {
#======
prob <- "Estadisticos de Orden"
cat(paste("\n",prob,":\n"))

ev <- function (x) return(eval(parse(text = x)))

N <- 2000        # Numero de simulaciones
n <- m <- 21    # taman~o de muestra

k <- 5     # estadistico de orden desead
if(k < 1 | k > n) stop("\n*Estadístico de orden fuera de rango!")

x <- rep(NA,N)

#   Leyes de Probabilidad
# 1 = binomial
# 2 = poisson
# 3 = uniforme
# 4 = normal
# 5 = gamma
ley <- 5
ley <- ifelse(ley == c(1,2,3,4,5),1,0)

if(ley[1]) {
LeyProb <-  "Distribución Binomial"
Nn <- 15
p <- 0.3
mtitle <- substitute("par?metros: "*N*"="*Nn*", "*P*" ="*p,
            list(N=quote(n),Nn=Nn,P=quote(p),p=p))
leyProb <- "rbinom(n,Nn,p)"

for(i in seq(N)) x[i] <- sort(ev(leyProb))[k]
}

if(ley[2]) {
LeyProb <-  "Distribución Poisson"
lambda <- 3
mtitle <- substitute("par?metro: "*Lambda*"="*lambda,
                list(Lambda=quote(lambda),lambda=lambda))
leyProb <- "rpois(n,lambda)"

for(i in seq(N)) x[i] <- sort(ev(leyProb))[k]
}

if(ley[3]) {
LeyProb <-  "Distribución Uniforme"
a <- 1
b <- 2
mtitle <- substitute("par?metros: "*A*"="*a*", "*B*" ="*b,
            list(A=quote(a),a=a,B=quote(b),b=b))
leyProb <- "runif(n,a,b)"

for(i in seq(N)) x[i] <- sort(ev(leyProb))[k]
}

if(ley[4]) {
LeyProb <-  "Distribución Normal"
mu <- 1
sigma <- 1
mtitle <- substitute("par?metros: "*Mu*"="*mu*", "*Sigma*" ="*sigma,
            list(Mu=quote(mu),mu=mu,Sigma=quote(sigma),sigma=sigma))
leyProb <- "rnorm(n,mu,sigma)"

for(i in seq(N)) x[i] <- sort(ev(leyProb))[k]
}

if(ley[5]) {
LeyProb <-  "Distribución Gamma"
alpha <- 1  
beta <- 1
mx=2
mtitle <- substitute("parámetros: "*Alpha*"="*alpha*", "*Beta*" ="*beta,
            list(Alpha=quote(alpha),alpha=alpha,Beta=quote(beta),beta=beta))
leyProb <- "rgamma(n,alpha,beta)"

for(i in seq(N)) x[i] <- sort(ev(leyProb))[k]
}

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2),mar=c(4,4,2,1),mgp=c(2,.5,0),oma=c(2,0,2,0),pty="s",
    cex=1,cex.axis=0.8,las=1)

n <- N
xlab <- "x"
mlab <- paste("distribución poblacional")
y <- ev(leyProb)
xlim <- range(y)
hist(y,probability=TRUE,xlim=xlim,
    border="white",col="bisque",
    xlab=xlab,ylab="densidad",main=mlab,cex.main=0.8)
if(which(ley==1)>2) lines(density(y),xlim=xlim,lwd=2,col="coral4")
points(y <- sample(y,m),rep(0,m), pch=20, cex=1)
points(sort(y)[k],0, pch=20, cex=1, col=2)

xlab <- expression(x[(k)])
mlab <- paste("k=",k," estadístico de orden",sep="")
hist(x,probability=TRUE,xlim=xlim,
    border="white",col="burlywood4",
    xlab=xlab,ylab="densidad",main=mlab,cex.main=0.8)
if(which(ley==1)>2) lines(density(x),xlim=xlim,lwd=2,col="coral4")

title(LeyProb,line=1,outer=TRUE)
title(mtitle,cex.main=0.9,line=0,outer=TRUE)
title(paste("Número de muestras simuladas:",N),cex.main=0.9,line=-2,outer=TRUE)
title(paste("Tamaño de muestra:",m),cex.main=0.8,line=-3,outer=TRUE)

par(opar)
#rm(list=ls())
#======
}
#=================================


