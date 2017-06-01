# PCA and Cluster Analysis
# James C. Bain & Libby Cowgill

# https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


library(ggplot2)
library(tidyr)
library(dplyr)
library(devtools)
library(ggbiplot)
library(cluster)
library(ggdendro)

file <- '~/Documents/research/neander_trauma/Data/contingency_norm.csv'
file2<- '~/Documents/research/neander_trauma/Data/neander_contingency_norm.csv'

activities <- read.csv(file)
neander <- read.csv(file2)

berger <- data.frame(X = c('bt5','libben','nubia','london','new york','new mexico','bt rodeo'),
                     head_neck = c(4, 6, 17, 108, 1640, 13, 71),
                     trunk = c(114,20,11,121,1469,99,18),
                     shoulder_arm = c(50,28,85,547,3025,183,47),
                     hand = c(14,0,3,421,2624,187,11),
                     pelvis = c(7,0,6,3,65,17,6),
                     leg = c(20,37,36,409,2466,88,11),
                     foot = c(14,3,2,121,670,205,17),
                     total = c(223,94,160,1730,11959,792,181)
                     )

berg_norm <-berger %>% 
  mutate(head_neck = head_neck/total,
         trunk = trunk/total,
         shoulder_arm = shoulder_arm/total,
         hand = hand/total,
         pelvis = pelvis/total,
         leg = leg/total,
         foot = foot/total) %>%
  select(X,head_neck,shoulder_arm,hand,pelvis,leg,foot,trunk)

df <- rbind(activities,neander)

df <- df %>%
  select(-c(`X.1`))

df <- rbind(df, berg_norm)
rownames(df) <- df$X




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


# Exploring

d <- dist(df[2:8])
act.hclust <- hclust(d)

group.25<-cutree(act.hclust,25)
pam.act <- pam(d,10)
k <- kmeans(df[2:8],10)

table(group.25, pam.act$clustering)
table(group.25,k$cluster)

plot(act.hclust,main='Default from hclust')

model <- agnes(d)
dg <- as.dendrogram(model)
ggdendrogram(dg)
#ggsave('~/Documents/research/neander_trauma/images/plots/agnes.png',width = 12)

cutree(model,25)
t <- table(cutree(model,25),group.25)
sum(diag(t))/sum(t)
