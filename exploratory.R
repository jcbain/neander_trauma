# Neanderthal Trauma vs. Sport Fracture Analysis
# NEISS Data
#
#
# James C. Bain & Dr. Libby Cowgill

setwd(dir = 'Desktop/Spring 2016/Neander_Trauma/')

library(plyr)
library(ggplot2)

sport <- read.csv('Data/NEISS/sport_final.csv')

count(sport, 'prod1')
aggregate(numeric(nrow(sport)), sport[c("body_part", "prod1")], length)
