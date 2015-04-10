library(dplyr)
library(reshape2)

############################################################
#
#
# Set 1
#
############################################################

trend_mean = 0.07
trend_sd = 0.01

mu <- 12
sig <- 0.3

trend <- rnorm(1,mean = trend_mean, trend_sd)

samples <- rlnorm(500,mu,sig)
years <- do.call("c", lapply(1:10,rep,times=50))

data_set_1 <- data.frame(year = years, loss = samples)
data_set_1$loss <- data_set_1$loss * (1 + trend)^(data_set_1$year - 1)

data_set_1$loss.id = 1:nrow(data_set_1)

write.csv(data_set_1, "c:/Projects/git/trend-estimation/data_set_1.csv", row.names=F)
write.csv(data.frame(trend = trend), "c:/Projects/git/trend-estimation/results/data_set_1_trend_rates.csv", row.names=F)



############################################################
#
#
# Set 2
#
# Random trend rates 
#
############################################################

n_years <- 10
freq_mean <- 41
mu <- 10
sig <- 0.27

trend_mean = 0.07
trend_sd = 0.01

trend_rates <- rnorm(n_years, mean = trend_mean, trend_sd)
cum_trend_rates <- cumprod(1 + trend_rates)

data_set_2 <- do.call("rbind", lapply(1:n_years, function(year){
  
  count <- rpois(1,lambda = freq_mean)
  
  samples <- rlnorm(count,mu,sig) * cum_trend_rates[year]
  
  retVal <- data.frame(year = year, loss = samples)
  
  
}))

data_set_2$loss.id = 1:nrow(data_set_2)

write.csv(data_set_2, "c:/Projects/git/trend-estimation/data_set_2.csv", row.names=F)

write.csv(data.frame(year = 1: length(trend_rates), trend=trend_rates), "c:/Projects/git/trend-estimation/results/data_set_2_trend_rates.csv", row.names=F)


############################################################
#
#
# Set 3
#
# Include True IBNR
# 
############################################################
n_years <- 10
freq_mean <- 20
mu <- 10
sig <- 0.27

trend_mean = 0.12
trend_sd = 0.02
ibnr_pattern <- c(0.4,0.6,0.75,0.85,0.92,0.96,0.99,1)

######################################################
#
#
# Function to randomise the IBNR pattern
#
######################################################
randomise_pattern <- function(pattern, cv = 0.2 ,max_propn = NA, min_propn = NA){
  
  n_cv <- as.numeric(cv)
  
  v <- c(0,pattern[1:(length(x = pattern) - 1)])
  out_of <- 1-v
  diffs <- pattern - v  
  
  props<-diffs/out_of
  
  retVal <- numeric()
  x<-numeric()
  for(i in 1:length(pattern)){
    
    prop <- rnorm(1,props[i], cv * abs(x = props[i]))
    if(!is.na(max_propn)){
      prop <- min(prop,max_propn)
    }
    
    if(!is.na(min_propn)){
      prop<-max(prop,min_propn)
    }
    
    x<- c(x,prop)
    
    value = ifelse(i==1, 0, retVal[i-1])
    
    value <- (1-value) * prop + value
    
    retVal <- c(retVal, value)
  }
  
  retVal[length(retVal)] = 1
  
  return(retVal)
  
}

# true ibnr applied to counts
counts <- rev(randomise_pattern(ibnr_pattern, max_propn = 1, min_propn = 0))
counts <- c(rep(1,times = max(0,n_years - length(counts))), counts)



trend_rates <- rnorm(n_years,mean = trend_mean, trend_sd)
cum_trend_rates <- cumprod(1 + trend_rates)
cum_trend_rates <- c(cum_trend_rates, rep(1,times = max(0,n_years - length(cum_trend_rates))))

data_set_3 <- do.call("rbind", lapply(1:n_years,function(year){
  
  trend<-trend_rates[year]
  count <- rpois(1,lambda = freq_mean)
  
  count_dev <- ceiling(count * counts[min(length(counts), year)])
  
  
  samples <- rlnorm(count_dev,mu,sig) * cum_trend_rates[year]
  
  retVal <- data.frame(year = year, loss = samples)
  
  
}))

data_set_3$loss.id = 1:nrow(data_set_3)

write.csv(data_set_3, "c:/Projects/git/trend-estimation/data_set_3.csv", row.names=F)
write.csv(data.frame(year = 1: length(counts), pattern = rev(counts)), "c:/Projects/git/trend-estimation/results/data_set_3_ibnr_pattern.csv", row.names=F)
write.csv(data.frame(year = 1: length(trend_rates), trend=trend_rates), "c:/Projects/git/trend-estimation/results/data_set_3_trend_rates.csv", row.names=F)
          
          
data_set_3 %>%
  group_by(year) %>%
  summarise(count = n())
trend_rates

############################################################
#
#
# Set 4
#
# Include True IBNR
# Include IBNER
# 
############################################################
n_years <- 14
ibnr_pattern <- c(0.4,0.6,0.75,0.85,0.92,0.96,0.99,1)

ibner_pattern <- c(0.7,0.8,0.85,0.9,0.94,0.97,1)

trend_mean = 0.04
trend_sd = 0.02

freq_mean <- 35
mu <- 13.0
sig <- 0.24

## ibnr
counts <- rev(randomise_pattern(ibnr_pattern, max_propn = 1, min_propn = 0))
counts <- c(rep(1,times = max(0,n_years - length(counts))), counts)



trend_rates <- rnorm(n_years,mean = trend_mean, trend_sd)
cum_trend_rates <- cumprod(1 + trend_rates)
cum_trend_rates <- c(cum_trend_rates, rep(1,times = max(0,n_years - length(cum_trend_rates))))

data_set_4_full_developed <- do.call("rbind", lapply(1:n_years,function(year){
  
  trend<-trend_rates[year]
  count <- rpois(1,lambda = freq_mean)
  
  count_dev <- ceiling(count * counts[min(length(counts), year)])
  
  
  samples <- rlnorm(count_dev,mu,sig) * cum_trend_rates[year]

  retVal <- data.frame(year = year, loss = samples)
  
  
}))

data_set_4_full_developed$loss.id = 1:nrow(data_set_4_full_developed)

#ibner
ibner <- do.call("rbind",by(data_set_4_full_developed, 
                            INDICES = 1:nrow(data_set_4_full_developed), 
                            FUN = function(row){
  
  rnd_ibner <- randomise_pattern(ibner_pattern)
  rnd_ibner <- c(rnd_ibner, rep(1, max(0, n_years - length(rnd_ibner))))
  
  retVal <- data.frame(loss.id = row$loss.id,year = row$year, dev = 1: n_years, ibner = rnd_ibner, stringsAsFactors = F)
  retVal <- retVal %>%
    filter(dev <= n_years - year + 1)
  
  retVal
  
}))


data_set_4 <- ibner %>%
  inner_join(data_set_4_full_developed, by = c("loss.id", "year")) %>%
  mutate(loss = loss * ibner) %>%
  select(-ibner) %>%
  dcast(formula = loss.id + year ~ dev, value.var = "loss")


write.csv(data_set_4, "c:/Projects/git/trend-estimation/data_set_4.csv", row.names=F)
write.csv(data_set_4_full_developed, "c:/Projects/git/trend-estimation/results/data_set_4_ultimate.csv", row.names=F)
write.csv(data.frame(pattern = rev(counts)), "c:/Projects/git/trend-estimation/results/data_set_4_ibnr_pattern.csv", row.names=F)
write.csv(data.frame(trend=trend_rates), "c:/Projects/git/trend-estimation/results/data_set_4_trend_rates.csv", row.names=F)


############################################################
#
#
# Set 5
#
# Include True IBNR
# Include IBNER
# Notification level
# 
############################################################
n_years <- 12
notification_level <- 750000

ibnr_pattern <- c(0.3, 0.5, 0.7, 0.8, 0.88, 0.95, 0.97, 0.99, 1)

ibner_pattern <- c(0.5, 0.7, 0.8, 0.95, 1.05, 1.1, 1.05, 1.02, 1.01, 1)

trend_mean = 0.09
trend_sd = 0.03

freq_mean <- 56
mu <- 13.3
sig <- 0.21

## ibnr
counts <- rev(randomise_pattern(ibnr_pattern, max_propn = 1, min_propn = 0))
counts <- c(rep(1,times = max(0,n_years - length(counts))), counts)



trend_rates <- rnorm(n_years,mean = trend_mean, trend_sd)
cum_trend_rates <- cumprod(1 + trend_rates)
cum_trend_rates <- c(cum_trend_rates, rep(1,times = max(0,n_years - length(cum_trend_rates))))

data_set_5_full_developed <- do.call("rbind", lapply(1:n_years,function(year){
  
  trend<-trend_rates[year]
  count <- rpois(1,lambda = freq_mean)
  
  count_dev <- ceiling(count * counts[min(length(counts), year)])
  
  
  samples <- rlnorm(count_dev,mu,sig) * cum_trend_rates[year]
  
  retVal <- data.frame(year = year, loss = samples)
  
  
}))

data_set_5_full_developed$loss.id = 1:nrow(data_set_5_full_developed)

#ibner
ibner <- do.call("rbind",by(data_set_5_full_developed, 
                            INDICES = 1:nrow(data_set_5_full_developed), 
                            FUN = function(row){
                              
                              rnd_ibner <- randomise_pattern(ibner_pattern,cv = 0.25)
                              rnd_ibner <- c(rnd_ibner, rep(1, max(0, n_years - length(rnd_ibner))))
                              
                              retVal <- data.frame(loss.id = row$loss.id,year = row$year, dev = 1: n_years, ibner = rnd_ibner, stringsAsFactors = F)
                              retVal <- retVal %>%
                                filter(dev <= n_years - year + 1)
                              
                              retVal
                              
                            }))


data_set_5 <- ibner %>%
  inner_join(data_set_5_full_developed, by = c("loss.id", "year")) %>%
  mutate(loss = loss * ibner) %>%
  select(-ibner) %>%
  group_by(loss.id)%>%
  mutate(max = max(loss)) %>%
  filter(max >= notification_level) %>%
  dcast(formula = loss.id + year ~ dev, value.var = "loss") %>%
  mutate(notification_level = notification_level)
  
# add if included to original data set


full.output <- ibner %>%
  dcast(formula = loss.id + year ~ dev, value.var = "ibner") %>%
  inner_join(data_set_5_full_developed, by = c("loss.id", "year")) %>%
  left_join(data_set_5 %>% select(loss.id, year) %>% mutate(reported = T), by = c("loss.id", "year")) %>%
  mutate(reported = ifelse(is.na(reported),F,T))
  
# remove loss.id from data set
data_set_5 <- data_set_5 %>%
  select(-loss.id)

write.csv(data_set_5, "c:/Projects/git/trend-estimation/data_set_5.csv", row.names=F)
write.csv(full.output, "c:/Projects/git/trend-estimation/results/data_set_5_ultimate.csv", row.names=F)
write.csv(data.frame(pattern = rev(counts)), "c:/Projects/git/trend-estimation/results/data_set_5_ibnr_pattern.csv", row.names=F)
write.csv(data.frame(trend=trend_rates), "c:/Projects/git/trend-estimation/results/data_set_5_trend_rates.csv", row.names=F)




############################################################
#
#
# Analysis
#
############################################################

analysis.data <- data_set_4 

analysis.data$loss <- analysis.data[,14-analysis.data$year+3]
p <- ggplot(data = analysis.data, aes(as.factor(year), loss))
p + geom_point(aes(color = as.factor(year)))

mean_plot <- ggplot(analysis.data, aes(as.factor(year), loss))
mean_plot + geom_boxplot()

analysis.data$log_loss <- log(analysis.data$loss)
analysis.data$year_offset <- analysis.data$year - 1
x <- lm(data = analysis.data, formula = log_loss ~ as.factor(year_offset) - 1)


############################################################
#
#
# IBNER Analysis
#
############################################################
p <- ggplot(ibner[1:500,], aes(dev,ibner))
p + geom_line(aes(color = as.factor(loss.id)))


d.data <- ibner %>%
  group_by(dev) %>% 
  summarise(ibner = mean(ibner))

p <- ggplot(d.data, aes(dev,ibner))
p + geom_line()



