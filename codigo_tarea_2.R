#codigos tarea 2
set.seed(2)
library(rjags)
library(coda)
library(readxl)
library(bookdown)
library(loo)
#1----
Ni <- c(1183,1510,1597,1924,1178,1324,2173,845,1450)
yi <- c(554,701,749,868,516,573,978,399,NA)
#priori A=10 y B=10
model_texto <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 10
    B <- 10
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    #modelo
    for (i in 1:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(mu,1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
       #log-verosimilitud
       ver[i] <- logdensity.bin(yi[i],pi[i], Ni[i]) 
    }

    
    #prediccion
    theta[9]~dnorm(mu,1/sigma^2)
    pi[9] <- exp(theta[9])/(1+exp(theta[9]))
    yi[9]~dbin(pi[9],Ni[9])
}"

model_data <- list(Ni = Ni,
                   yi = yi, N = 8)

model <- jags.model(textConnection(model_texto), 
                     data = model_data,n.chains = 3
                     ,n.adapt = 500)
update( model, n.iter=500 )

mcmc_samples= coda.samples(model , 
                             variable.names=c("pi","theta"
                                              ,"yi","ver")
                             ,n.iter=10000 )
summary(mcmc_samples)



par(mfrow = c(1, 2))
plot(density(mcmc_samples2[[1]][,1]),main = "Densidad de P")
plot(ts(mcmc_samples[[1]][,1]),xlab = "", ylab = "",main="Traceplot de P")


par(mfrow = c(2, 2))
plot(ts(mcmc_samples[[1]][,1]),xlab = "", ylab = "",main="Traceplot de P_1")
plot(ts(mcmc_samples[[1]][,2]),xlab = "", ylab = "",main="Traceplot de P_2")
plot(ts(mcmc_samples[[1]][,3]),xlab = "", ylab = "",main="Traceplot de P_3")
plot(ts(mcmc_samples[[1]][,4]),xlab = "", ylab = "",main="Traceplot de P_4")
par(mfrow = c(2, 2))
plot(ts(mcmc_samples[[1]][,5]),xlab = "", ylab = "",main="Traceplot de P_5")
plot(ts(mcmc_samples[[1]][,6]),xlab = "", ylab = "",main="Traceplot de P_6")
plot(ts(mcmc_samples[[1]][,7]),xlab = "", ylab = "",main="Traceplot de P_7")
plot(ts(mcmc_samples[[1]][,8]),xlab = "", ylab = "",main="Traceplot de P_8")


par(mfrow = c(1, 1))
plot(density(mcmc_samples[[1]][,1]),xlim=c(0.395,0.525)
     ,ylim=c(0,60),main = "Densidades de cada Pi",col="darkslategray")
lines(density(mcmc_samples[[1]][,2]), col = "red")
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples[[1]][,8]), col = "cadetblue3")


#2---- 
Ni <- c(1183,1510,1597,1924,1178,1324,2173,845,1450)
yi <- c(554,701,749,868,516,573,978,399,NA)

model_texto2 <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 10
    B <- 10
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi, Ni[i]) 
       #log_verosimilitud
      ver[i] <- logdensity.bin(yi[i],pi, Ni[i])  }
    theta~dnorm(mu,1/sigma^2)
    pi<- exp(theta)/(1+exp(theta))
    yi[9] ~ dbin(pi, Ni[9]) 
}"

model_data2 <- list(Ni = Ni,
                   yi = yi, N = 8)

model2 <- jags.model(textConnection(model_texto2), 
                    data = model_data2,n.chains = 3
                    ,n.adapt = 500 )
update( model2 , n.iter=500 )
mcmc_samples2= coda.samples(model2 , 
                           variable.names=c("pi","theta",
                                            "yi","ver")
                           ,n.iter=10000 )


summary(mcmc_samples2)
plot(density(mcmc_samples2[[1]][,1]))
lines(density(mcmc_samples[[1]][,2]), col = "red",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,6]), col = "#458B00",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1",lty=2,lwd=0.5)
lines(density(mcmc_samples[[1]][,8]), col = "indianred1",lty=2,lwd=0.5)



#3----

Ni <- c(1183,1510,1597,1924,1178,1324,2173,845,1450)
yi <- c(554,701,749,868,516,573,978,399,NA)
model_texto3 <- "model{
    #hiperparametros
    #C es la varianza de theta[1]
    C <- 10
    D <- 10
    
    #priori
    theta[1] ~ dnorm(0,1/C)
    pi[1] <- exp(theta[1])/(1+exp(theta[1]))
    ver[1] <- logdensity.bin(yi[1],pi[1], Ni[1]) 
    sigma ~ dunif(0,D)
    
    for (i in 2:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(theta[i-1],1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
              
       #log-verosimilitud
       ver[i] <- logdensity.bin(yi[i],pi[i], Ni[i]) 
    }
    #predicción
    yi[9] ~ dbin(pi[9], Ni[9]) 
    theta[9]~dnorm(theta[8],1/sigma^2)
    pi[9] <- exp(theta[9])/(1+exp(theta[9]))
}"

model_data3 <- list(Ni = Ni,
                   yi = yi, N = 8)

model3 <- jags.model(textConnection(model_texto3), 
                    data = model_data3,n.chains = 3
                    ,n.adapt = 500)
update( model3 , n.iter=500 )
mcmc_samples3= coda.samples( model3 , 
                            variable.names=c("pi","theta",
                                             "yi","ver")
                            ,n.iter=10000 )
par(mfrow = c(2, 2))
plot(ts(mcmc_samples3[[1]][,1]),xlab = "", ylab = "",main="Traceplot de P_1")
plot(ts(mcmc_samples3[[1]][,2]),xlab = "", ylab = "",main="Traceplot de P_2")
plot(ts(mcmc_samples3[[1]][,3]),xlab = "", ylab = "",main="Traceplot de P_3")
plot(ts(mcmc_samples3[[1]][,4]),xlab = "", ylab = "",main="Traceplot de P_4")
par(mfrow = c(2, 2))
plot(ts(mcmc_samples3[[1]][,5]),xlab = "", ylab = "",main="Traceplot de P_5")
plot(ts(mcmc_samples3[[1]][,6]),xlab = "", ylab = "",main="Traceplot de P_6")
plot(ts(mcmc_samples3[[1]][,7]),xlab = "", ylab = "",main="Traceplot de P_7")
plot(ts(mcmc_samples3[[1]][,8]),xlab = "", ylab = "",main="Traceplot de P_8")

par(mfrow = c(1, 1))
plot(density(mcmc_samples3[[1]][,1]),ylim=c(0,60)
     ,xlim=c(0.39,0.525),main = "Densidades de Pi",col="darkslategray")
lines(density(mcmc_samples3[[1]][,2]), col = "red")
lines(density(mcmc_samples3[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples3[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples3[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples3[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples3[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples3[[1]][,8]), col = "cadetblue3")
#4----
summary(mcmc_samples3)
summary(mcmc_samples2)
summary(mcmc_samples)

library(coda)
library(ggplot2)

## ggplot de y
samples_yi9_model1 <- as.array(mcmc_samples[[1]][,35])
samples_yi9_model2 <- as.array(mcmc_samples2[[1]][,19])
samples_yi9_model3 <- as.array(mcmc_samples3[[1]][,35])

df <- data.frame(
  Model = c("thetas indep", "thetas ident", "theta dep.anterior"),
  Estimate = c(mean(samples_yi9_model1), mean(samples_yi9_model2), mean(samples_yi9_model3)),
  Lower = c(HPDinterval(as.mcmc(samples_yi9_model1))[1], HPDinterval(as.mcmc(samples_yi9_model2))[1], HPDinterval(as.mcmc(samples_yi9_model3))[1]),
  Upper = c(HPDinterval(as.mcmc(samples_yi9_model1))[2], HPDinterval(as.mcmc(samples_yi9_model2))[2], HPDinterval(as.mcmc(samples_yi9_model3))[2])
)

ggplot(df, aes(y = Model)) +
  geom_segment(aes(x = Lower, xend = Upper, yend = Model), color = "red") +
  geom_point(aes(x = Estimate)) +labs(x = "Cantidad de exitos",
                                      title = "Predicción de tiros convertidos 2007") +
  theme_minimal()+  theme(axis.title.y = element_blank(),
                          plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(590, 750),breaks = seq(from = 600, to = 730, by = 40))






### ggplot de P

samples_yi9_model1 <- as.array(mcmc_samples[[1]][,9])
samples_yi9_model2 <- as.array(mcmc_samples2[[1]][,1])
samples_yi9_model3 <- as.array(mcmc_samples3[[1]][,9])

df <- data.frame(
  Model = c("thetas indep", "thetas ident", "theta dep.anterior"),
  Estimate = c(mean(samples_yi9_model1), mean(samples_yi9_model2), mean(samples_yi9_model3)),
  Lower = c(HPDinterval(as.mcmc(samples_yi9_model1))[1], HPDinterval(as.mcmc(samples_yi9_model2))[1], HPDinterval(as.mcmc(samples_yi9_model3))[1]),
  Upper = c(HPDinterval(as.mcmc(samples_yi9_model1))[2], HPDinterval(as.mcmc(samples_yi9_model2))[2], HPDinterval(as.mcmc(samples_yi9_model3))[2])
)

ggplot(df, aes(y = Model)) +
  geom_segment(aes(x = Lower, xend = Upper, yend = Model), color = "red") +
  geom_point(aes(x = Estimate)) +labs(x = "Probabilidad de exito",
                                      title = "Predicción de probabilidad de encestar 2007") +
  theme_minimal()+  theme(axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))






library(rjags)
##c----

suppressWarnings(waic(as.matrix(mcmc_samples[[1]][,19:26])))
suppressWarnings(waic(as.matrix(mcmc_samples2[[1]][,3:10])))
suppressWarnings(waic(as.matrix(mcmc_samples3[[1]][,19:26])))



logLikelihood1 <- sum(colMeans(mcmc_samples[[1]][,19:26]))

BIC_value1 <- -2 * logLikelihood1 + 10 * log(80000)


logLikelihood2 <- sum(colMeans(mcmc_samples2[[1]][,3:10]))

BIC_value2 <- -2 * logLikelihood2 + 3 * log(80000)


logLikelihood3 <- sum(colMeans(mcmc_samples3[[1]][,19:26]))

BIC_value3 <- -2 * logLikelihood3 + 9 * log(80000)

c(BIC_value1,BIC_value2,BIC_value3)

#se elige modelo 2





