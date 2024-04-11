#codigos tarea 2

#1----
library(rjags)
library(readxl)
library(bookdown)

pi <-c(46.8,46.4,46.9,45.1,43.8,43.3,45.0,47.2)
Ni <- c(1183,1510,1597,1924,1178,1324,2173,845)
yi <- c(554,701,749,868,516,573,978,399)
#priori A=100 y B=100
model_texto <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 100
    B <- 100
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(mu,1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
    }

}"

model_data <- list(Ni = Ni,
                   yi = yi, N = 8)

model <- jags.model(textConnection(model_texto), 
                     data = model_data,n.chains = 1
                     ,n.adapt = 500)

mcmc_samples= coda.samples(model , 
                             variable.names=c("pi")
                             ,n.iter=6000 )
summary(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]),xlim=c(0.395,0.55)
     ,ylim=c(0,60))
lines(density(mcmc_samples[[1]][,2]), col = "red")
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples[[1]][,8]), col = "indianred1")


##otras prioris
#priori A=1 y B=1
model_texto <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 1
    B <- 1
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(mu,1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
    }

}"

model_data <- list(Ni = Ni,
                   yi = yi, N = 8)

model <- jags.model(textConnection(model_texto), 
                    data = model_data,n.chains = 1
                    ,n.adapt = 500)

mcmc_samples= coda.samples(model , 
                           variable.names=c("pi")
                           ,n.iter=6000 )
summary(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]),xlim=c(0.395,0.55)
     ,ylim=c(0,60))
lines(density(mcmc_samples[[1]][,2]), col = "red")
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples[[1]][,8]), col = "indianred1")

##otras prioris
#priori A=1 y B=100
model_texto <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 1
    B <- 100
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(mu,1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
    }

}"

model_data <- list(Ni = Ni,
                   yi = yi, N = 8)

model <- jags.model(textConnection(model_texto), 
                    data = model_data,n.chains = 1
                    ,n.adapt = 500)

mcmc_samples= coda.samples(model , 
                           variable.names=c("pi")
                           ,n.iter=6000 )
summary(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]),xlim=c(0.395,0.525)
     ,ylim=c(0,60))
lines(density(mcmc_samples[[1]][,2]), col = "red")
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples[[1]][,8]), col = "indianred1")



#A=100 y B=1
model_texto <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 100
    B <- 1
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(mu,1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
    }

}"

model_data <- list(Ni = Ni,
                   yi = yi, N = 8)

model <- jags.model(textConnection(model_texto), 
                    data = model_data,n.chains = 1
                    ,n.adapt = 500)

mcmc_samples= coda.samples(model , 
                           variable.names=c("pi")
                           ,n.iter=6000 )
summary(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]),xlim=c(0.395,0.525)
     ,ylim=c(0,60))
lines(density(mcmc_samples[[1]][,2]), col = "red")
lines(density(mcmc_samples[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples[[1]][,8]), col = "indianred1")

#aun no se como elegir

plot(density(mcmc_samples[[1]]),xlim=c(0.395,0.525)
     ,ylim=c(0,60))
#2---- 
model_texto2 <- "model{
    #hiperparametros
    #A es la varianza de mu
    A <- 100
    B <- 1
    
    #priori
    mu ~ dnorm(0,1/A)
    sigma ~ dunif(0,B)
    
    for (i in 1:N){
       yi[i] ~ dbin(pi, Ni[i]) 
    }
    theta~dnorm(mu,1/sigma^2)
    pi<- exp(theta)/(1+exp(theta))
}"

model_data2 <- list(Ni = Ni,
                   yi = yi, N = 8)

model2 <- jags.model(textConnection(model_texto2), 
                    data = model_data2,n.chains = 1
                    ,n.adapt = 500)

mcmc_samples2= coda.samples(model2 , 
                           variable.names=c("pi")
                           ,n.iter=6000 )
summary(mcmc_samples2)
plot(density(mcmc_samples2[[1]][,1]))



#3----

Ni <- c(1183,1510,1597,1924,1178,1324,2173,845)
yi <- c(554,701,749,868,516,573,978,399)
model_texto3 <- "model{
    #hiperparametros
    #C es la varianza de theta[1]
    C <- 1
    D <- 1
    
    #priori
    theta[1] ~ dnorm(0,1/C)
    pi[1] <- exp(theta[1])/(1+exp(theta[1]))
    sigma ~ dunif(0,D)
    
    for (i in 2:N){
       yi[i] ~ dbin(pi[i], Ni[i]) 
       theta[i]~dnorm(theta[i-1],1/sigma^2)
       pi[i] <- exp(theta[i])/(1+exp(theta[i]))
    }
}"

model_data3 <- list(Ni = Ni,
                   yi = yi, N = 8)

model3 <- jags.model(textConnection(model_texto3), 
                    data = model_data3,n.chains = 1
                    ,n.adapt = 500)

mcmc_samples3= coda.samples( model3 , 
                            variable.names=c("pi")
                            ,n.iter=6000 )
summary(mcmc_samples3)

plot(density(mcmc_samples3[[1]][,1]),xlim=c(0.395,0.525)
     ,ylim=c(0,60))
lines(density(mcmc_samples3[[1]][,2]), col = "red")
lines(density(mcmc_samples3[[1]][,3]), col = "#0000CD")
lines(density(mcmc_samples3[[1]][,4]), col = "burlywood3")
lines(density(mcmc_samples3[[1]][,5]), col = "chocolate2")
lines(density(mcmc_samples3[[1]][,6]), col = "#458B00")
lines(density(mcmc_samples3[[1]][,7]), col = "deeppink1")
lines(density(mcmc_samples3[[1]][,8]), col = "indianred1")




#4----







