library(dplyr)
library(reshape2)
library(ggplot2)
library(runjags)
library(coda)

data.4 <- read.csv("../data_set_4.csv", stringsAsFactors=F)



quantile_results <- function(data){
  years <- unique(data$year)
  
  retVal = do.call("rbind", lapply(years,function(yr){
    
    year_losses <- data %>% filter(year == yr) %>% select(loss) %>% unlist()
    
    quantiles <- quantile(year_losses,probs = c(0.1,0.25,0.5,0.75,0.9))
    
    result <- data.frame(as.list(quantiles))
    result$year <- yr
    
    return(result)
  }))
  
  return(retVal)
}


x <- quantile_results(data.4)


means <- data.4 %>%
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

####################################################################################
# JAGS script 
####################################################################################
#
# JAGS script
#
modelString = "model {
  mu[1]<-alpha[w[1]]+beta[d[1]]

  logloss[1]~dnorm(mu[1],1/sig2[1])

  for (i in 2:length(w)){
    mu[i]<-alpha[w[i]] + beta[d[i]] + rho*(logloss[i-1]-mu[i-1])*wne1[i]
    logloss[i]~dnorm(mu[i],1/sig2[i])
  }
  #
  # set up sig2
  #
  for (i in 1:length(w)){
  sig2[i]<-sigd2[d[i]]
  }
  for (j in 1:10){
  sigd2[j]<-sum(a[j:10])
  }
  for (k in 1:10){
  a[k]~dunif(0.000001,1)
  }
  #
  # specify priors
  #
  for (i in 1:numlev){
  alpha[i]~dnorm(log(premium[i])+logelr,.1)
  }
  logelr~dunif(-1.5,0.5)
  #
  for (i in 1:9){
    beta[i]~dunif(-5,5)
  }
  beta[10]<-0 
  # rho~dunif(-1,1)
  rho~dunif(-.00001,.00001) # Use for LCL model

  ltrend ~ dunif(-1,1)
  
  tmu[i] <- mu + ltrend*(year[i] - 1)    #mu is the log mean for year 8 


}" 


#
# initialize JAGS model
#
inits1=list(.RNG.name= "base::Wichmann-Hill",
            .RNG.seed= 12441)
inits2=list(.RNG.name= "base::Marsaglia-Multicarry",
            .RNG.seed= 12442)
inits4=list(.RNG.name= "base::Super-Duper",
            .RNG.seed= 12444)
inits4=list(.RNG.name= "base::Mersenne-Twister",
            .RNG.seed= 12444)

data.for.jags=list(lclaims = log(data.4$loss),
                   year = data.4$year,
                   loss = data.4$loss)
#
# run the model
# #

jagout=run.jags(model=modelString,monitor=c("ltrend","mu","sigma"),
                data=data.for.jags,n.chains=4,method="parallel",
                inits=list(inits1,inits2,inits4,inits4),thin=10,
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
