## simulating some data to illustrate the code

## simulate data with normal residuals
n= 100
ID <- rep(1:n)
intercept <- rep(5,n) 
treat = c(rep(0,n/2),rep(3, n/2))
treatBYtime1 = rep(c(0,5), each=n/2)
treatBYtime2 = rep(c(0,6), each=n/2)
treatBYtime3 = rep(c(0,7), each=n/2)
ID_effect = rnorm(n, 0, 5)
resid = rnorm(4*n, 0, 3)

data <- as.data.frame(ID)
data$T0 <- intercept+resid[1:n] 
data$T1 <- intercept+ treat + treatBYtime1+ID_effect+resid[(n+1):(2*n)] 
data$T2 <- intercept+ treat + treatBYtime2+ID_effect+resid[(2*n+1):(3*n)] 
data$T3 <- intercept+ treat + treatBYtime3+ID_effect+resid[(3*n+1):(4*n)] 
data$group <- rep(c('sham', 'treat'), each=n/2) 

##############################################################################

## Start of mixed model analysis code

## load required libraries

## library to model mixed models
# library(lme4) # basic library
library(lmerTest) # extended functionality 
## library for plots
library(ggplot2)
## library for data wrangling
library(tidyverse)
## library for normality assumption checking
library(car)

## load in the data set

## from a csv file
#data <- read.csv("Y:/MyDocs/resources/test data sets/data.csv")

## each row is the observation of one particular study subject
## will use ID as a random effect in a model predicting outcome scores from treatment group and time point.
## outcomes are listed in wide format with a separate column for each time point
## reorganize data into long format using pivot_longer
## we want to have columns for outcome = score, ID, time point, and treatment type
data2 <- pivot_longer(data, names_to = "time_point", values_to = "score", cols = c(2:5))

## setting up the linear mixed model
m <- lmer(score ~ time_point + group + time_point:group + (1|ID), data = data2, REML = F)

## effect estimates table
summary(m)

## creating an ANOVA table
anova(m)

## residual plots to check for normality of residuals
# histogram - look for gaussian shape
hist(residuals(m))
# qqplot
qqnorm(residuals(m))
qqline(residuals(m))
# qqplot with confidence bounds (car library function)
qqPlot(residuals(m))

## plotting the data
# box and whisker plot
data2 %>% ggplot(aes(x = time_point, y = score, color = group))+
                   geom_boxplot()

# mean and SE over time
sum_data <- data2 %>% 
  group_by(time_point, group) %>%
  summarise(mean_score = mean(score), 
            sd = sd(score),
            n = n(),
            SE = sd / sqrt(n))

sum_data %>% ggplot(aes(x = time_point, y = mean_score, color = group))+
  geom_errorbar(aes(ymin=mean_score-SE, ymax=mean_score+SE), width=.1)+
  geom_line(aes(group = group))+
  geom_point()

