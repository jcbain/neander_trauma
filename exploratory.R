# Neanderthal Trauma vs. Sport Fracture Analysis
# NEISS Data
#
#
# James C. Bain & Dr. Libby Cowgill

setwd(dir = 'Desktop/Spring 2016/Neander_Trauma/')

## load packages ##
library(MASS)
library(plyr)
library(ggplot2)

## read in the file ##
sport <- read.csv('Data/NEISS/sport_final.csv')

## aggregate the data by body part of the facture and cause ##
count(sport, 'prod1')
aggregate(numeric(nrow(sport)), sport[c("body_part", "prod1")], length)

#####################################
## chi square test of independence ##
#####################################

## create a contigency table of sport and injury location ##
tbl <- table(sport$prod1,sport$body_part)
tbl

## run the test ##
chisq.test(tbl) 
# calculate effect sizes

tableMatrix<-as.data.frame.matrix(tbl)
tableMatrix$totals<-unname(rowSums(tableMatrix[1:nrow(tableMatrix),])) # find the total fractures per sport and assign it to a colmn

sum(tableMatrix[1,])
