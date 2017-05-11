# PCA and Cluster Analysis
# James C. Bain & Libby Cowgill

# https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


library(ggplot2)
library(tidyr)
library(dplyr)
library(devtools)
library(ggbiplot)

file <- '~/Documents/research/neander_trauma/Data/contingency_norm.csv'

df <- read.csv(file)

rownames(df) <- df$X


df <- df %>%
  select(-c(`X.1`))

k <- kmeans(df[2:8],6, nstart = 25)

df$cluster <- k$cluster


df %>%
  filter(cluster == 5) %>%
  select(`X`)

s <-scale(df[,2:8])
# log transform 
log.ir <- log(df[, 2:8])
ir.cluster <- df$cluster

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(s,
                 center = TRUE,
                 scale. = TRUE) 
plot(ir.pca, type = "l")

g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.cluster, ellipse = TRUE, 
              circle = TRUE)
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')


clusters <- hclust(dist(df[,2:8]))
plot(clusters)
