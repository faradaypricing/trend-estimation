library(dplyr)
library(reshape2)
library(ggplot2)
library(runjags)
library(coda)

data.1 <- read.csv("../data_set_1.csv", stringsAsFactors=F)



quantile_results <- function(data){
  years <- unique(data.1$year)
  
  retVal = do.call("rbind", lapply(years,function(yr){
    
    year_losses <- data %>% filter(year == yr) %>% select(loss) %>% unlist()
    
    quantiles <- quantile(year_losses,probs = c(0.1,0.25,0.5,0.75,0.9))
    
    result <- data.frame(as.list(quantiles))
    result$year <- yr
  
    return(result)
  }))

  return(retVal)
}


x <- quantile_results(data.1)


means <- data.1 %>%
  group_by(year) %>%
  summarise(loss = mean(loss))


plot.data <- x %>% melt(id.vars = c("year"), variable.name = "quantile", value.name = "loss") %>%
  rbind(means %>% mutate(quantile = "mean"))

p <- ggplot(plot.data, aes(year, loss))
p <- p + geom_line(aes(color=quantile))
p

lmeans <- means %>% mutate(lmean = log(loss)) %>% select(-loss)

m <- lm(data = lmeans, formula = lmean ~ year)

m$coefficients[['year']]

mean <- mean(data.1$loss)
sd <- sd(data.1$loss)

shape <- sd*sd / mean
rate <- shape/mean
####################################################################################
# JAGS script 
####################################################################################
modelString = "model {
for (i in 1:500){
  tmu[i] <- mu + ltrend*(year[i] - 1)    #mu is the log mean for year 8 

  lclaims[i] ~ dnorm(tmu[i],1/sigma^2)

}
#
# specify priors
#
  ltrend ~ dunif(-1,1)
  mu ~ dunif(0,35)
  sigma ~ dunif(0,1)
}" 
#
# initialize JAGS model
#
inits1=list(.RNG.name= "base::Wichmann-Hill",
            .RNG.seed= 12341)
inits2=list(.RNG.name= "base::Marsaglia-Multicarry",
            .RNG.seed= 12342)
inits3=list(.RNG.name= "base::Super-Duper",
            .RNG.seed= 12343)
inits4=list(.RNG.name= "base::Mersenne-Twister",
            .RNG.seed= 12344)

data.for.jags=list(lclaims = log(data.1$loss),
                   year = data.1$year,
                   loss = data.1$loss)
#
# run the model
# #

jagout=run.jags(model=modelString,monitor=c("ltrend","mu","sigma"),
                data=data.for.jags,n.chains=4,method="parallel",
                inits=list(inits1,inits2,inits3,inits4),thin=10,
                plots=TRUE,burnin=5000,sample=2500,psrf.target=1.05)
print(jagout$timetaken)

plot(jagout)

gelman=gelman.diag(jagout)
maxpsrf=max(gelman$psrf[,1])
print(paste("maxpsrf =",maxpsrf))

samples <- as.matrix(jagout$mcmc)

jag.means <- sapply(1:10, function(i){
  exp(mean(samples[,2] + samples[,1] * (i - 1)))
})
df.lnorm <- data.frame(x=1:10,y=jag.means,quantile="lnorm.jag.mean")
jag.means

########################################################################################
# GAMMA MODEL
########################################################################################

mean <- mean(data.1$loss)
sd <- sd(data.1$loss)

shape <- sd*sd / mean
rate <- shape/mean
####################################################################################
# JAGS script 
####################################################################################
modelString.gamma = "model {
for (i in 1:500){

  rt[i] <- rate/(trend + 1)^(year[i] - 1)
  loss[i] ~ dgamma(shape, rt[i])

}

trend ~ dunif(-1,1)
shape ~ dunif(10000,50000)
rate ~ dunif(0.000001,0.25)
}" 
#
# initialize JAGS model
#
inits1=list(.RNG.name= "base::Wichmann-Hill",
            .RNG.seed= 12341)
inits2=list(.RNG.name= "base::Marsaglia-Multicarry",
            .RNG.seed= 12342)
inits3=list(.RNG.name= "base::Super-Duper",
            .RNG.seed= 12343)
inits4=list(.RNG.name= "base::Mersenne-Twister",
            .RNG.seed= 12344)

data.for.jags=list(lclaims = log(data.1$loss),
                   year = data.1$year,
                   loss = data.1$loss)
#
# run the model
# #

jagout.gamma =run.jags(model=modelString.gamma,monitor=c("rate","shape","trend"),
                data=data.for.jags,n.chains=4,method="parallel",
                inits=list(inits1,inits2,inits3,inits4),thin=10,
                plots=TRUE,burnin=5000,sample=2500,psrf.target=1.05)
print(jagout.gamma$timetaken)


plot(jagout.gamma)

gelman=gelman.diag(jagout.gamma)
maxpsrf=max(gelman$psrf[,1])
print(paste("maxpsrf =",maxpsrf))

samples <- as.matrix(jagout.gamma$mcmc)


jag.means <- sapply(1:10, function(i){
  mean(samples[,2] * (1+samples[,3]) ^ (i - 1) / samples[,1])
})
df <- data.frame(x=1:10,y=jag.means,quantile="gamma.jag.mean")

p2 <- p +geom_line(data = df, aes(x,y))
p2


########################################################################################
# UNIFORM MODEL
########################################################################################

####################################################################################
# JAGS script 
####################################################################################
modelString.unif = "model {
for (i in 1:500){

  tmu[i] <- mu + ltrend * (year[i] - 1)
  lclaims[i] ~ dunif(tmu[i] - 4 * sig, tmu[i] + 4 * sig)

}

ltrend ~ dunif(-1,1)
sig ~ dlnorm(0.0001,2)
mu ~ dunif(7,15)
}" 
#
# initialize JAGS model
#
inits1=list(.RNG.name= "base::Wichmann-Hill",
            .RNG.seed= 12341)
inits2=list(.RNG.name= "base::Marsaglia-Multicarry",
            .RNG.seed= 12342)
inits3=list(.RNG.name= "base::Super-Duper",
            .RNG.seed= 12343)
inits4=list(.RNG.name= "base::Mersenne-Twister",
            .RNG.seed= 12344)

data.for.jags=list(lclaims = log(data.1$loss),
                   year = data.1$year,
                   loss = data.1$loss)
#
# run the model
# #

jagout.unif =run.jags(model=modelString.unif,monitor=c("ltrend","mu","sig"),
                       data=data.for.jags,n.chains=4,method="parallel",
                       inits=list(inits1,inits2,inits3,inits4),thin=20,
                       plots=TRUE,burnin=10000,sample=2500,psrf.target=1.05)
print(jagout.unif$timetaken)


plot(jagout.unif)

gelman=gelman.diag(jagout.unif)
maxpsrf=max(gelman$psrf[,1])
print(paste("maxpsrf =",maxpsrf))

samples <- as.matrix(jagout.unif$mcmc)


jag.unif.means <- sapply(1:10, function(i){
  exp(mean(samples[,2] + samples[,1] * (i - 1)))
})
df.unif <- data.frame(x=1:10,y=jag.unif.means,quantile="unif.jag.mean")

p3 <- p2 + geom_line(data = df.unif, aes(x,y))
p3

p4 <- p3 + geom_line(data = df.lnorm, aes(x,y), color="blue")
p4
df.lnorm

