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
sport <- read.csv('~/Documents/research/neander_trauma/Data/NEISS/sport_final.csv')

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

################################################
## create a dataframe of the contigency table ##
################################################

tableMatrix<-as.data.frame.matrix(tbl) ## change the table into an intuitive dataframe
tableMatrix$totals<-unname(rowSums(tableMatrix[1:nrow(tableMatrix),])) # find the total fractures per sport and assign it to a colmn
props<-tableMatrix[1:length(tableMatrix)-1]/tableMatrix$totals # turn the table into a proportional table

rowSums(props[1:nrow(props),]) ## verify that I took the proportion by seeing all values add to one

## welp, looks like I need to redo this for plotting...
## didn't think that through
## I need to find a way to aggregate the sport dataframe by proportion


###########
## plots ##
###########
ggplot(head(df),aes(x = factor(Var1),y=Freq)) + geom_point(aes(color=Var2)) 
