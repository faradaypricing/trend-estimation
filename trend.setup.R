library(dplyr)
library(randomForest)
############################################################
#
#
# Set 1
#
############################################################


trend <- rnorm(1,mean = 0.05, 0.02)

mu <- 12
sig <- 0.3
samples <- rlnorm(500,mu,sig)
years <- do.call("c", lapply(1:10,rep,times=50))

data_set_1 <- data.frame(year = years, loss = samples)
data_set_1$loss <- data_set_1$loss * (1 + trend)^(data_set_1$year - 1)

write.csv(data_set_1, "~/git/data_set_1.csv")



############################################################
#
#
# Set 2
#
############################################################

freq_mean <- 12
mu <- 10
sig <- 0.27

data_set_2 <- do.call("rbind", lapply(1:10,function(year){
  
  trend <- rnorm(1,mean = 0.07, 0.01)
  count <- rpois(1,lambda = freq_mean)
  
  samples <- rlnorm(count,mu,sig) * (1+trend)^year
  
  retVal <- data.frame(year = year, loss = samples, trend = trend)
  
  
}))

write.csv(data_set_2, "~/git/data_set_2.csv")







############################################################
#
#
# Analysis
#
############################################################

analysis.data <- data_set_2
p <- ggplot(data = analysis.data, aes(as.factor(year), loss))
p + geom_point(aes(color = as.factor(year)))

mean_plot <- ggplot(analysis.data, aes(as.factor(year), loss))
mean_plot + geom_boxplot()

analysis.data$log_loss <- log(analysis.data$loss)
analysis.data$year_offset <- analysis.data$year - 1
x <- lm(data = analysis.data, formula = log_loss ~ as.factor(year_offset) - 1)

forest <- randomForest()


x$coefficients
exp(summary(x)$coefficients)
results <- exp(summary(x)$coefficients[,1]) / exp(summary(x)$coefficients[1,1])

summary(x)

counts <- analysis.data %>%
  group_by(year) %>%
  summarise(count = n(), trend = max(trend))

var(counts$count)
mean(counts$count)

mean(counts$trend)
