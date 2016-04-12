setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

library(plyr)
library(ggplot2)
library(plotly)
library(reshape)

# read in the data
df <- read.csv('Data/NEISS/sport_category_final.csv')

# create a contigency table 
orig_table <- table(df$prod1,df$body_part)

# create a datafame from the contigency table (at least one that is intuitive)
tm<-as.data.frame.matrix(orig_table) # tm stands for table matrix

# create new columns based on body part combinations
head_neck <- tm$face + tm$neck + tm$head
shoulder_arm <- tm$`upper arm` + tm$`lower arm` + tm$shoulder + tm$elbow
hand <- tm$hand + tm$finger + tm$wrist
pelvis <- tm$`pubic region` + tm$hip # look up Berger femoral neck categorization and within the data
leg <- tm$knee + tm$`lower leg` + tm$`upper leg`
foot <- tm$foot + tm$toe + tm$ankle
trunk <- tm$`upper trunk` + tm$back


# combine columns into a new dataframe
final<-as.data.frame(cbind(head_neck,shoulder_arm,hand,pelvis,leg,foot,trunk))
rownames(final)<-rownames(tm) # index will be the activity name

# recreate data from Berger Trinkaus papers
neander_tot<-c(8,4,7,1,1,3,3)
nea_wo_djd<-c(7,4,7,1,0,3,1)
nea_wo_shan1<-c(6,4,5,1,1,3,1)
nea_wo_shan1_djd<-c(5,4,5,1,0,3,0)

# apply chi square for neander total and every sport
nt<-t(apply(final,1,function(x) {
  new<- cbind(neander_tot,x)
  ch <- chisq.test(new)
  c(unname(ch$statistic), ch$p.value)}))

nwd<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_djd,x)
  ch <- chisq.test(new)
  c(unname(ch$statistic), ch$p.value)}))

nws<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_shan1,x)
  ch <- chisq.test(new)
  c(unname(ch$statistic), ch$p.value)}))

nwsd<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_shan1_djd,x)
  ch <- chisq.test(new)
  c(unname(ch$statistic), ch$p.value)}))


# create a function to cleanup the chi square tables and be able to pull out different objects from this cleanup
chi2cleanup<-function(table){
  
  # read in one of the chi square tables (nt, nwd, nws or nwsd)
  frame<-as.data.frame(table,row.names = rownames(table))
  names(frame) = c('X2','P-Value')
  fin = na.omit(frame) 
  
  # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
  c<- list(
    rowna=frame[is.nan(frame$X2),],   # $rowna 
    final=fin,                        # $final
    similar=fin[fin$`P-Value` > .05,] # $similar (activities that are similar, P > 0.05)
  )
  return(c)
}

# name cleaned up chi square tables
n_tot<-chi2cleanup(nt)  # neanderthal total
n_djd<-chi2cleanup(nwd) # neanderthal w/o djd
n_s<-chi2cleanup(nws)   # neanderthal w/o shan
n_sd<-chi2cleanup(nwsd) # neanderthal w/o shan or djd


# find similar activities per sample 
n_tot$similar
n_djd$similar
n_s$similar
n_sd$similar
 
# create a function to find the similar activities frome the original contigency table
similarSelectorCounts<-function(frame,final_frame,neander_sample){ 
  
  require(reshape)
  
  indices=rownames(frame$similar)               # find rows to set as indices
  new_rows = final_frame[indices,]              # map those to original contigency table
  
  sample =append("neander",rownames(new_rows))  # add a new vector to use a "sample" column
  
  joined_rows = rbind(neander_sample,new_rows)  # join rows from neanderthal sample to new dataframe
  joined_cols = cbind(sample,joined_rows)       # add the "sample" column
  rownames(joined_cols) = NULL                  # remove the row indices 
  
  melted <- melt(joined_cols, id=(c("sample"))) # transpose contigency table
  
  return(melted)                
}

# like similarSelectorCounts() but returns proportions instead
similarSelector<-function(frame,final_frame,neander_sample){ 
  
  require(reshape)
  require(plyr)
  indices=rownames(frame$similar)               # find rows to set as indices
  new_rows = final_frame[indices,]              # map those to original contigency table
  
  sample =append("neander",rownames(new_rows))  # add a new vector to use a "sample" column
  
  joined_rows = rbind(neander_sample,new_rows) # join rows from neanderthal sample to new dataframe
  props = prop.table(as.table(as.matrix(joined_rows)),1)

  props<-as.data.frame.matrix(props)
  joined_cols = cbind(sample,props)       # add the "sample" column
  rownames(joined_cols) = NULL                  # remove the row indices 
  
  melted <- melt(joined_cols, id=(c("sample"))) # transpose contigency table
  
  return(melted)                
}

# an example of how similarSelector works
n<-similarSelector(n_tot,final,neander_tot)
d<-similarSelector(n_djd,final,nea_wo_djd)
s<-similarSelector(n_s,final,nea_wo_shan1)
sd<-similarSelector(n_sd,final,nea_wo_shan1_djd)

# extract just the neanderthal row for emphasis in plots
n2<-n[n$sample=='neander',]
d2<-d[d$sample=='neander',]
s2<-s[s$sample=='neander',]
sd<-sd[sd$sample=='neander',]

# plotting 
ggplot(data=n, 
       aes(x=factor(variable), y=value, 
           group=sample,
           color=sample)) + 
  geom_line() + 
  geom_point() +
  geom_point(data=n2,aes(x=factor(variable), y=value, 
                         group=sample, size = 4))+
  geom_line(data=n2,aes(x=factor(variable), y=value, 
                        group=sample, size = 2)) 
  scale_x_discrete("Proportion") +
  scale_y_continuous("Body Part")
