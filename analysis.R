setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

#######################
library(plyr)       # #
library(ggplot2)    # #
library(plotly)     # #
library(reshape)    # #
library(vcd)        # #
library(DescTools)  # #
#######################

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

## apply chi square for neander total and every sport

# nt<-t(apply(final,1,function(x) {
#   new<- cbind(neander_tot,x)
#   ch <- chisq.test(new)
#   c(unname(ch$statistic), ch$p.value)}))

##############################
######### CRAMER'S V #########
##############################

## this is an example with confidence intervals however,
## this was buggy and perhaps unnecessary

#tots<-t(apply(final,1,function(x) {
#  new<- cbind(neander_tot,x)
#  ch <- chisq.test(new)
#  chi<-c(unname(ch$statistic), ch$p.value)
#  cram<-CramerV(new,conf.level=0.90)
#  cbind(chi,cram)
#}))


tots<-t(apply(final,1,function(x) {
  new<- cbind(neander_tot,x)
  ch <- chisq.test(new)
  chi<-c(unname(ch$statistic), ch$p.value)
  cram<-CramerV(new)
  cbind(chi,cram)
}))


nodjd<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_djd,x)
  ch <- chisq.test(new)
  chi<-c(unname(ch$statistic), ch$p.value)
  cram<-CramerV(new)
  cbind(chi,cram)
}))

noshan<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_shan1,x)
  ch <- chisq.test(new)
  chi<-c(unname(ch$statistic), ch$p.value)
  cram<-CramerV(new)
  cbind(chi,cram)
}))


noshandjd<-t(apply(final,1,function(x) {
  new<- cbind(nea_wo_shan1_djd,x)
  ch <- chisq.test(new)
  chi<-c(unname(ch$statistic), ch$p.value)
  cram<-CramerV(new)
  cbind(chi,cram)
}))


####################################################################################################################
## create a function to cleanup the chi square tables and be able to pull out different objects from this cleanup ##
####################################################################################################################
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

# like chi2cleanup but with cramer's v and confidence intervals
chi2cleanup2<-function(table){
  
  # read in one of the chi square tables (nt, nwd, nws or nwsd)
  frame<-as.data.frame(table,row.names = rownames(table))
  frame<-frame[-c(3)] # drop the extra p value column
  names(frame) = c('X2','P-Value','Cramers V', 'l.conf','u.conf')
  fin = na.omit(frame) 
  
  # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
  c<- list(
    rowna=frame[is.nan(frame$X2),],   # $rowna 
    final=fin,                        # $final
    similar=fin[fin$`P-Value` > .05,] # $similar (activities that are similar, P > 0.05)
  )
  return(c)
}


# like chi2cleanup but with cramer's v 
chi2cleanup3<-function(table){
  
  # read in one of the chi square tables (nt, nwd, nws or nwsd)
  frame<-as.data.frame(table,row.names = rownames(table))
  frame<-frame[-c(4)] # drop the extra p value column
  names(frame) = c('X2','P-Value','Cramers V')
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

# cleaned up with cramer's v
e<-chi2cleanup2(noshan)

# find similar activities per sample 
n_tot$similar
n_djd$similar
n_s$similar
n_sd$similar

#############################################################
## finding activities that are not statistically different ##
#############################################################
 
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
simToNeanderTotal<-similarSelector(n_tot,final,neander_tot)
simToNeanderWOdjd<-similarSelector(n_djd,final,nea_wo_djd)
simToNeanderWOShan<-similarSelector(n_s,final,nea_wo_shan1)
simToNeanderWOdjdOrShan<-similarSelector(n_sd,final,nea_wo_shan1_djd)

# extract just the neanderthal row for emphasis in plots
n2<-simToNeanderTotal[simToNeanderTotal$sample=='neander',]
d2<-simToNeanderWOdjd[simToNeanderWOdjd$sample=='neander',]
s2<-simToNeanderWOShan[simToNeanderWOShan$sample=='neander',]
sd<-simToNeanderWOdjdOrShan[simToNeanderWOdjdOrShan$sample=='neander',]

######################################################################
## plotting ##########################################################
######################################################################
ggplot(data=simToNeanderTotal, # this needs to be one of the dataframes directly above
       aes(x=factor(variable), y=value, 
           group=sample,
           color=sample)) + 
  geom_line() + 
  geom_point() +
  geom_point(data=n2,aes(x=factor(variable), y=value, 
                         group=sample, size = 4))+
  geom_line(data=n2,aes(x=factor(variable), y=value, 
                        group=sample, size = 2))+ 
  scale_x_discrete("Proportion") +
  scale_y_continuous("Body Part")+
  guides(size=FALSE)



