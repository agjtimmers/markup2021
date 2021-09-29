##########################################
## Assignment 1 Markup Languages #########
## Annemarie Timmers (6238106) ###########
##########################################

set.seed(1009) 

#draw samples
nsamples <- 100
samples <- matrix(0, 10000, nsamples)
for (i in 1:nsamples) {
  samples[, i] <- rnorm(10000)
}

#make function that extracts the required information
stats <- function(x){
  meanx <- mean(x)
  abias <- abs(meanx - 0)
  se <- sd(x) / sqrt(length(x))
  df <- length(x) - 1
  error <- qt(0.975, df) * se
  lower <- meanx - error
  upper <- meanx + error
  return(c(meanx, abias, se, lower, upper))
}

#actually extract the information and put it in a dataframe
info <- apply(samples, 2, stats) #apply the function to all samples
info <- as.data.frame(t(info)) #get the information in the columns
colnames(info) <- c("mean", "absolute bias", "standard error", "lower", "upper") #assign names

#make variable if pop value is in the confidence interval
info <- info %>%
  mutate(covered = lower < 0 & 0 < upper) 
mean(info$covered) #check the coverage

#create a plot that demonstrates the statement of Neyman
library(ggplot2)
ggplot(info, aes(1:100, mean, col = covered)) + #different color denotes if the ci contains the pop mean
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + #add confidence interval to the points
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("samples") + #change names on the axes
  ylab("sample mean") 

#table containing all simulated samples for which the confidence interval does not contain the population value
library(knitr)
table <- kable(info[info$covered == F, ]) #nice table
table


#try to change something

