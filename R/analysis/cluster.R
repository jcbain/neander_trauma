# PCA and Cluster Analysis
# James C. Bain & Libby Cowgill

# 


library(ggplot2)
library(tidyr)
library(dplyr)
library(devtools)
library(ggbiplot)

file <- '~/Documents/research/neander_trauma/Data/contingency_norm.csv'

df <- read.csv(file)

#rownames(df) <- df$X

df <- df %>%
  select(-c(`X.1`))

k <- kmeans(df[2:8],6, nstart = 25)

df$cluster <- k$cluster


df %>%
  filter(cluster == 5) %>%
  select(`X`)


# log transform 
log.ir <- log(df[, 2:8])
ir.cluster <- df$cluster

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(df[,2:7],
                 center = TRUE,
                 scale. = TRUE) 
plot(ir.pca, type = "l")

g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.cluster, ellipse = TRUE, 
              circle = TRUE)
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
