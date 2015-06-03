library(dplyr)
library(reshape2)
library(ggplot2)
library(runjags)
library(coda)

data.3 <- read.csv("../data_set_3.csv", stringsAsFactors=F)



quantile_results <- function(data){
  years <- unique(data.3$year)
  
  retVal = do.call("rbind", lapply(years,function(yr){
    
    year_losses <- data %>% filter(year == yr) %>% select(loss) %>% unlist()
    
    quantiles <- quantile(year_losses,probs = c(0.1,0.25,0.5,0.75,0.9))
    
    result <- data.frame(as.list(quantiles))
    result$year <- yr
    
    return(result)
  }))
  
  return(retVal)
}


x <- quantile_results(data.3)


means <- data.3 %>%
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
modelString = "model {
for (i in 1:length(lclaims)){
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

data.for.jags=list(lclaims = log(data.3$loss),
                   year = data.3$year,
                   loss = data.3$loss)
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
