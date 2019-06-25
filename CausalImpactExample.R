# Load packages and set working directory
library(CausalImpact)
library(ggplot2)
library(zoo)
setwd("//dkhm0881/xdrive4/DKS0378520/Documents/R_Files/CausalImpactForRUsers")

# Import data
# Column 1 is dates, column 2 is FootwearLineA, columns 3-8 are correlated data series
dat <- read.csv("CausalImpactExample.csv", stringsAsFactors = FALSE)
summary(dat)

# Set pre/post time periods
dates <- seq.Date(as.Date("2018-08-18"), by = 1, length.out = 120)
pre.dates <- as.Date(c("2018-08-18", "2018-10-31"))
post.dates <- as.Date(c("2018-11-01", "2018-12-15")) # Advertising ended 2018-11-30, but lingering effects possible

# Check correlations of data for pre period
pre.dat <- dat[1:75,2:8]
cor(pre.dat, method = c("pearson"))

# Format data as a zoo time series object
dat <- dat[,2:8]
dat <- zoo(dat, dates)

# Causal impact test
impact <- CausalImpact(data = dat, 
                       pre.period = pre.dates, 
                       post.period = post.dates, 
                       alpha = 0.05 # Defaults to 95% intervals
                       )

# Causal impact results
summary(impact) # Statistical results
summary(impact, "report") # Report

# Plot results
plot(impact)

# Alter post period to end when advertising campaign ends and rerun
post.dates <- as.Date(c("2018-11-01", "2018-11-30"))
impact2 <- CausalImpact(data = dat, 
                       pre.period = pre.dates, 
                       post.period = post.dates, 
                       alpha = 0.05 # Defaults to 95% intervals
                       )
summary(impact2)
summary(impact2, "report")

# Plot results
plot(impact2)

# Can adjust plot using usual functions from ggplot2
plot(impact2) + 
  ggtitle("Advertising Impact on FootwearLineA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Revenue") +
  scale_y_continuous(labels=scales::dollar) 

# Check which correlated data series were most likely to be included in counterfactual
plot(impact2$model$bsts.model, "coefficients")

