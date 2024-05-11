#
# Datos SAT, tomado de Gelman et al. Sección 5.5.
# Datos y análisis preliminar
#
School <- LETTERS[1:8]
y <- c(28,8,-3,7,-1,1,18,12) # efecto de tratamientos
se <- c(15,10,16,11,9,11,10,18) # desviaciones estándar, se asumen conocidas
#
#
# Primero: usando prioris planas independientes p(theta_i) = 1 se obtiene
#     theta_i | data ~ N(ybar[i],se[i])
#
x.musep <- seq(-50,80,length=250)
postmu.sep <- matrix(0,nrow=8,ncol=length(x.musep))
for (i in 1:8) postmu.sep[i,] <- dnorm(x.musep,mean=y[i],sd=se[i])
par(lwd=2)
plot(x.musep,postmu.sep[1,],pch="",xlab="Efecto",ylim=range(postmu.sep),ylab="")
for (i in 1:8) lines(x.musep,postmu.sep[i,],col=i)
legend("topright",legend=School,text.col=1:8,bty="n")
#
# Bajo el supuesto theta_1=...=theta_8 podemos agrupar todos los datos:
#
postmu.comb <- sum(y/se^2)/sum(1/se^2)
postsd.comb <- sqrt(1/sum(1/se^2))
x <- seq(-30,60,length=250)
post95 <- round(qnorm(c(0.025,0.975),mean=postmu.comb,sd=postsd.comb),
digits=3)
plot(x,dnorm(x,mean=postmu.comb,sd=postsd.comb),type="l",
xlab="Theta",ylab="Posteriori de theta, modelo agrupado",
main=paste("IC 95%  = (",post95[1],",",post95[2],")"))
lines(post95,c(0,0),lwd=3)
for (i in 1:8) lines(x.musep,postmu.sep[i,],lty=2,lwd=0.5)
#
# Podemos también realizar test frecuentista de H0: theta_1=..=theta_J
# suponiendo n1=..=nJ=n, and sigma1=..=sigmaJ=sigma/n
#
ybar <- mean(y)
sumsq <- sum((y-ybar)^2/se^2) #  ~ chisq(7) bajo H0
p.chsq <- 1-pchisq(sumsq,df=7)
x <- seq(0,20,length=250)
plot(x,dchisq(x,df=7),type="l",xlab="",ylab="Densidad Chi^2(7)",
main=paste("Valor p = ",round(p.chsq,digits=3),sep=" "))
abline(v=sumsq,col=2,lty=2)
#
# Ahora el análisis Bayesian bajo modelo jerárquico
#
#   y_j ~ N(theta_j,sigma^2_j),    sigma_j^2 conocido
#   theta_1,...,theta_J ~ N(mu,tau^2)
#
# Suponemos primero p(mu,tau^2) = p(mu) p(tau^2)
#    y mu ~ N(0,10^4), y tau ~ U(0,10^2)
# esta es una priori propia pero vaga.
#
# NOTA: Stan usa parametrización N(mean, s.d.)!!
#
library(StanHeaders)
if(! "rstan" %in% tolower((.packages()))){
  library("rstan")
  (.packages())
}
if(! "KernSmooth" %in% tolower((.packages()))){
  library("KernSmooth")
  (.packages())
}

N <- length(y)
df1 <- list(N=N, y=y, se=se)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'SAT-vague.stan',
    data = df1, chains = 3, verbose = TRUE,
    iter = 10000, control=list(adapt_delta = 0.9999, max_treedepth = 15))
print(fit.1)
#
# Resúmenes de la Posteriori
#
plot(fit.1,pars=c("theta","mu","tau"))
stan_hist(fit.1, pars=c("mu", "tau"),bins=50)

chains.1 <- extract(fit.1, permuted = TRUE)
mu.1 <- chains.1$mu
tau.1 <- chains.1$tau
theta.1 <- chains.1$theta

bwmu.1 <- density(mu.1)$bw*2
bwtau.1 <- density(tau.1)$bw*2
est.1 <- bkde2D(cbind(mu.1,tau.1),
bandwidth=c(bwmu.1,bwtau.1),
range.x=list(c(-5,25),c(0,25)))
contour(est.1$x1,est.1$x2,est.1$fhat,
xlab=expression(mu),
ylab=expression(tau),nlevels=15,drawlabels=F,
xlim=c(-5,25),ylim=c(0,25))
#
# Perfiles E(theta_i | tau,datos)
#    Nota: E(theta | tau,datos) = E(E(theta | mu,tau,datos))
#
seq.tau1 <- seq(0.01,50,length=500)
profiles.1 <- matrix(0,nrow=8,ncol=length(seq.tau1))

for (i in 1:8) {
    for (j in 1:length(seq.tau1)) {
        profiles.1[i,j] <- mean((y[i]/se[i]^2+mu.1/seq.tau1[j]^2)/(1/se[i]^2+1/seq.tau1[j]^2))
    }
}

plot(seq.tau1,profiles.1[1,],pch="",ylim=c(-10,40),
xlab=expression(tau),ylab="E(theta_i | tau,datos)",main="Priori Vaga")
for (i in 1:8) lines(seq.tau1,profiles.1[i,],col=i)
legend("topleft",legend=School,text.col=1:8,bty="n",cex=0.8)
#
# Perfiles sd(theta_i | tau,datos). Nota:
# Var(theta | tau,datos ) = E(Var(theta | mu,tau,datos))
#         + Var(E(theta | mu,tau,datos))
#
seq.tau1 <- seq(0.01,50,length=500)
profsd.1 <- matrix(0,nrow=8,ncol=length(seq.tau1))

for (i in 1:8) {
  for (j in 1:length(seq.tau1)) {
    profsd.1[i,j] <- sqrt(mean(1/(1/se[i]^2+1/seq.tau1[j]^2)) +
      var((y[i]/se[i]^2+mu.1/seq.tau1[j]^2)/(1/se[i]^2+1/seq.tau1[j]^2)))
  }
}

plot(seq.tau1,profsd.1[1,],pch="",ylim=c(0,20),main="Priori Vaga",
     xlab=expression(tau),ylab="SD(theta_i | tau,datos)")
for (i in 1:8) lines(seq.tau1,profsd.1[i,],col=i)
legend("topleft",legend=School,text.col=1:8,bty="n",cex=0.8)

#
# Usamos ahora otra priori (mismo modelo muestral)
#
#   mu,tau^2 ~ p(mu,tau)=p(mu|tau)p(tau)
#          con p(mu|tau) = 1 (posteriori es propia)
#
#  usando diferentes versiones de p(tau)
#
# Nota: p(log(tau))=1 o equivalentemente, p(tau^2)=1/tau^2
#       implica posteriori impropia!!
#
#       Pero p(tau)=1 implica posteriori propia! Ver Gelman et al. Sección 5.4
#
N <- length(y)
df2 <- list(N=N, y=y, se=se)

options(mc.cores = parallel::detectCores())
fit.2 <- stan(file = 'SAT-flat.stan', data = df2,
               chains = 3, verbose = TRUE, iter = 5000,
               control=list(adapt_delta = 0.9999, max_treedepth = 15))
print(fit.2)
#
# Resúmenes de la Posteriori
#
plot(fit.2,pars=c("theta","mu","tau"))
stan_hist(fit.2, pars=c("mu", "tau"),bins=50)

chains.2 <- extract(fit.2, permuted = TRUE)
mu.2 <- chains.2$mu
tau.2 <- chains.2$tau
theta.2 <- chains.2$theta

bwmu.2 <- density(mu.2)$bw*2
bwtau.2 <- density(tau.2)$bw*2
est.2 <- bkde2D(cbind(mu.2,tau.2),
                bandwidth=c(bwmu.2,bwtau.2),
                range.x=list(c(-5,25),c(0,25)))
contour(est.2$x1,est.2$x2,est.2$fhat,
        xlab=expression(mu),
        ylab=expression(tau),nlevels=15,drawlabels=F,
        xlim=c(-5,25),ylim=c(0,25))
#
# Perfiles E(theta_i | tau,datos)
#
seq.tau1 <- seq(0.01,50,length=500)
profiles.2 <- matrix(0,nrow=8,ncol=length(seq.tau1))

for (i in 1:8) {
  for (j in 1:length(seq.tau1)) {
    profiles.2[i,j] <- mean((y[i]/se[i]^2+mu.2/seq.tau1[j]^2)/(1/se[i]^2+1/seq.tau1[j]^2))
  }
}

plot(seq.tau1,profiles.2[1,],pch="",ylim=c(-10,40),main="Priori Plana",
     xlab=expression(tau),ylab="E(theta_i | tau,datos)")
for (i in 1:8) lines(seq.tau1,profiles.2[i,],col=i)
legend("topleft",legend=School,text.col=1:8,bty="n",cex=0.8)
#
# Perfiles sd(theta_i | tau,datos). Note:
#
seq.tau1 <- seq(0.01,50,length=500)
profsd.2 <- matrix(0,nrow=8,ncol=length(seq.tau1))

for (i in 1:8) {
  for (j in 1:length(seq.tau1)) {
    profsd.2[i,j] <- sqrt(mean(1/(1/se[i]^2+1/seq.tau1[j]^2)) +
                            var((y[i]/se[i]^2+mu.2/seq.tau1[j]^2)/(1/se[i]^2+1/seq.tau1[j]^2)))
  }
}

plot(seq.tau1,profsd.2[1,],pch="",ylim=c(0,20), main="Priori Plana",
     xlab=expression(tau),ylab="SD(theta_i | tau,datos)")
for (i in 1:8) lines(seq.tau1,profsd.2[i,],col=i)
legend("topleft",legend=School,text.col=1:8,bty="n",cex=0.8)

#
# Ahora p(tau) = 1/(1+tau^2/A), i.e. half-Cauchy
#
A <- 2500
df3 <- list(N=N, y=y, se=se, A=A)

options(mc.cores = parallel::detectCores())
fit.3 <- stan(file = 'SAT-halfc.stan', data = df3, chains = 3,
              verbose = TRUE, iter = 50000,
              control=list(adapt_delta = 0.9999, max_treedepth = 15))
print(fit.3)
#
# Resúmenes de la Posteriori
#
plot(fit.3,pars=c("theta","mu","tau"))
stan_hist(fit.3, pars=c("mu", "tau"),bins=50)

chains.3 <- extract(fit.3, permuted = TRUE)
mu.3 <- chains.3$mu
tau.3 <- chains.3$tau
theta.3 <- chains.3$theta

bwmu.3 <- density(mu.3)$bw*2
bwtau.3 <- density(tau.3)$bw*2
est.3 <- bkde2D(cbind(mu.3,tau.3),
                bandwidth=c(bwmu.3,bwtau.3),
                range.x=list(c(-5,25),c(0,25)))
contour(est.3$x1,est.3$x2,est.3$fhat,
        xlab=expression(mu),
        ylab=expression(tau),nlevels=15,drawlabels=F,
        xlim=c(-5,25),ylim=c(0,25))
#
# Perfiles E(theta_i | tau,datos)
#
seq.tau1 <- seq(0.01,50,length=500)
profiles.3 <- matrix(0,nrow=8,ncol=length(seq.tau1))

for (i in 1:8) {
  for (j in 1:length(seq.tau1)) {
    profiles.3[i,j] <- mean((y[i]/se[i]^2+mu.3/seq.tau1[j]^2)/(1/se[i]^2+1/seq.tau1[j]^2))
  }
}

plot(seq.tau1,profiles.3[1,],pch="",ylim=c(-10,40),main="Half-Cauchy",
     xlab=expression(tau),ylab="E(theta_i | tau,datos)")
for (i in 1:8) lines(seq.tau1,profiles.3[i,],col=i)
legend("topleft",legend=School,text.col=1:8,bty="n")
#
# Comparamos las Inferencias
#
plot(density(tau.2,from=0,to=30,adj=1.2),type="n",xlab=expression(tau),main="")
lines(density(tau.1,from=0,to=30,adj=1.2),col=1)
lines(density(tau.2,from=0,to=30,adj=1.2),col=2)
lines(density(tau.3,from=0,to=30,adj=1.2),col=3)
legend("topright",legend=c("Priori Vaga","Priori Plana","Half Cauchy"),
       text.col=1:3,bty="n")

plot(density(mu.2,adj=1.4),type="n",xlab=expression(mu),main="")
lines(density(mu.1,adj=1.4),col=1)
lines(density(mu.2,adj=1.4),col=2)
lines(density(mu.3,adj=1.4),col=3)
legend("topleft",legend=c("Priori Vaga","Priori Plana","Half Cauchy"),
       text.col=1:3,bty="n")
#
# Comparamos ahora inferencias para los theta_i
#
sum.t1 <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) sum.t1[,i] <- quantile(theta.1[,i],probs=c(0.025,0.5,0.975))

sum.t2 <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) sum.t2[,i] <- quantile(theta.2[,i],probs=c(0.025,0.5,0.975))

sum.t3 <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) sum.t3[,i] <- quantile(theta.3[,i],probs=c(0.025,0.5,0.975))

sum.t0 <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) sum.t0[,i] <- qnorm(mean=y[i],sd=se[1],p=c(0.025,0.5,0.975))

plot(c(1:8),sum.t1[1,],pch="",xlab=expression(theta),ylab="",
     ylim=range(c(sum.t0,sum.t1,sum.t2,sum.t3)),
     main="Mediana e Intervalos de Credibilidad Centrales 95%")
for (i in 1:3) {
  points(1:8-0.15,sum.t0[2,],col=1,lwd=2)
  points(1:8-0.05,sum.t1[2,],col=2,lwd=2)
  points(1:8+0.05,sum.t2[2,],col=3,lwd=2)
  points(1:8+0.15,sum.t3[2,],col=4,lwd=2)
  for (j in 1:8) {
    lines(c(j-0.15,j-0.15),c(sum.t0[1,j],sum.t0[3,j]),col=1)
    lines(c(j-0.05,j-0.05),c(sum.t1[1,j],sum.t1[3,j]),col=2)
    lines(c(j+0.05,j+0.05),c(sum.t2[1,j],sum.t2[3,j]),col=3)
    lines(c(j+0.15,j+0.15),c(sum.t3[1,j],sum.t3[3,j]),col=4)
  }
}
legend("top",legend=c("Caso Independ.","Priori Vaga","Priori Plana","Half Cauchy"),
       text.col=1:4,bty="n",cex=0.8)
#############
###  DIC  ###
#############
theta1.B <- colMeans(theta.1)
theta2.B <- colMeans(theta.2)
theta3.B <- colMeans(theta.3)
logpt1.B <- sum(dnorm(y,mean=theta1.B,sd=se,log=TRUE))
logpt2.B <- sum(dnorm(y,mean=theta2.B,sd=se,log=TRUE))
logpt3.B <- sum(dnorm(y,mean=theta3.B,sd=se,log=TRUE))
print(-2*c(logpt1.B,logpt2.B,logpt3.B))
aux <- matrix(0,nrow=5,ncol=N)
for (i in 1:N) {
    aux[1,i] <- mean(dnorm(y[i],mean=theta.1[,i],sd=se[i],log=TRUE))
    aux[2,i] <- mean(dnorm(y[i],mean=theta.2[,i],sd=se[i],log=TRUE))
    aux[3,i] <- mean(dnorm(y[i],mean=theta.3[,i],sd=se[i],log=TRUE))
}
elogp1 <- sum(aux[1,])
elogp2 <- sum(aux[2,])
elogp3 <- sum(aux[3,])
pDIC.1 <- logpt1.B - elogp1
pDIC.2 <- logpt2.B - elogp2
pDIC.3 <- logpt3.B - elogp3

print(c(pDIC.1,pDIC.2,pDIC.3))
DIC.1 <- -2*logpt1.B + 2*pDIC.1
DIC.2 <- -2*logpt2.B + 2*pDIC.2
DIC.3 <- -2*logpt3.B + 2*pDIC.3
print(c(DIC.1,DIC.2,DIC.3))
#
############
### WAIC ###
############
aux <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) {
    aux[1,i] <- log(mean(dnorm(y[i],mean=theta.1[,i],sd=se[i])))-
    mean(dnorm(y[i],mean=theta.1[,i],sd=se[i],log=TRUE))
    aux[2,i] <- log(mean(dnorm(y[i],mean=theta.2[,i],sd=se[i])))-
    mean(dnorm(y[i],mean=theta.2[,i],sd=se[i],log=TRUE))
    aux[3,i] <- log(mean(dnorm(y[i],mean=theta.3[,i],sd=se[i])))-
    mean(dnorm(y[i],mean=theta.3[,i],sd=se[i],log=TRUE))
}
aux <- 2*aux
pWAIC1.1 <- sum(aux[1,])
pWAIC1.2 <- sum(aux[2,])
pWAIC1.3 <- sum(aux[3,])
print(c(pWAIC1.1,pWAIC1.2,pWAIC1.3))

aux <- rep(0,8)
for (i in 1:8) aux[i] <- var(dnorm(y[i],mean=theta.1[,i],sd=se[i],log=TRUE))
pWAIC2.1 <- sum(aux)
aux <- rep(0,8)
for (i in 1:8) aux[i] <- var(dnorm(y[i],mean=theta.2[,i],sd=se[i],log=TRUE))
pWAIC2.2 <- sum(aux)
aux <- rep(0,8)
for (i in 1:8) aux[i] <- var(dnorm(y[i],mean=theta.3[,i],sd=se[i],log=TRUE))
pWAIC2.3 <- sum(aux)
print(c(pWAIC2.1,pWAIC2.2,pWAIC2.3))

aux <- matrix(0,nrow=3,ncol=8)
for (i in 1:8) {
    aux[1,i] <- mean(dnorm(y[i],mean=theta.1[,i],sd=se[i]))
    aux[2,i] <- mean(dnorm(y[i],mean=theta.2[,i],sd=se[i]))
    aux[3,i] <- mean(dnorm(y[i],mean=theta.3[,i],sd=se[i]))
}
lppd1 <- sum(log(aux[1,]))
lppd2 <- sum(log(aux[2,]))
lppd3 <- sum(log(aux[3,]))

print(-2*c(lppd1,lppd2,lppd3))

WAIC1.1 <- -2*(lppd1-pWAIC1.1)
WAIC1.2 <- -2*(lppd2-pWAIC1.2)
WAIC1.3 <- -2*(lppd3-pWAIC1.3)
print(c(WAIC1.1,WAIC1.2,WAIC1.3))
WAIC2.1 <- -2*(lppd1-pWAIC2.1)
WAIC2.2 <- -2*(lppd2-pWAIC2.2)
WAIC2.3 <- -2*(lppd3-pWAIC2.3)
print(c(WAIC2.1,WAIC2.2,WAIC2.3))
############
### LPML ###
############
CPO.1 <- rep(0,N)
for (i in 1:N) CPO.1[i] <- 1/mean(1/dnorm(y[i],mean=theta.1[,i],sd=se[i]))
LPML.1 <- sum(log(CPO.1))

CPO.2 <- rep(0,N)
for (i in 1:N) CPO.2[i] <- 1/mean(1/dnorm(y[i],mean=theta.2[,i],sd=se[i]))
LPML.2 <- sum(log(CPO.2))

CPO.3 <- rep(0,N)
for (i in 1:N) CPO.3[i] <- 1/mean(1/dnorm(y[i],mean=theta.3[,i],sd=se[i]))
LPML.3 <- sum(log(CPO.3))
print(c(LPML.1,LPML.2,LPML.3))

