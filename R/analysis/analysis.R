setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

#######################
library(plyr)       # #
library(ggplot2)    # #
library(plotly)     # #
library(reshape)    # #
library(vcd)        # #
library(DescTools)  # #
library(dplyr)      # #
#######################

# read in the data
df <- read.csv('~/Documents/research/neander_trauma/Data/NEISS/sport_category_final.csv')

# create a contigency table 
orig_table <- table(df$prod1,df$body_part)

# create a datafame from the contigency table (at least one that is intuitive)
tm<-as.data.frame.matrix(orig_table) # tm stands for table matrix

# create new columns based on body part combinations
head_neck <- tm$face + tm$neck + tm$head
shoulder_arm <- tm$`upper arm` + tm$`lower arm` + tm$shoulder + tm$elbow
hand <- tm$hand + tm$finger + tm$wrist
pelvis <- tm$`pubic region` + tm$hip 
leg <- tm$knee + tm$`lower leg` + tm$`upper leg`
foot <- tm$foot + tm$toe + tm$ankle
trunk <- tm$`upper trunk` + tm$back


# combine columns into a new dataframe
final<-as.data.frame(cbind(head_neck,shoulder_arm,hand,pelvis,leg,foot,trunk))
rownames(final)<-rownames(tm) # index will be the activity name


# recreate data from Berger Trinkaus papers
sample1<-c(8,7,1,1,3,3,4) # total sample
sample2<-c(7,7,1,0,3,1,4) # without djd
sample3<-c(6,5,1,1,3,1,4) # without shandidar 1
sample4<-c(5,5,1,0,3,0,4) # without djd or shandidar 1

##############################
######### CRAMER'S V #########
##############################

neander_total_stat_table<-t(apply(final,1,function(x) {
  two_rows    =   cbind(sample1,x)                      # compare each row with neanderthal sample
  cramer_stat =   CramerV(two_rows,conf.level=0.90)     # perform cramer's v and find confidence intervals
  chi2test    =   chisq.test(two_rows)                  # perform chi square
  chi2_cols   =   c(unname(chi2test$statistic), chi2test$p.value)   # grab chi square and p-value
  frame       =   cbind(cramer_stat,chi2_cols)          # bind cramer's v, conf intervals, chi square and p-value together
  final_frame =   frame[-c(6)]                          # drop stupid extra chi square column
  names(final_frame) = c("cramer's v", "l.ci", "u.ci", "chi square", "p-value")   # name columns
  return(final_frame)                                   # return frame
}))

final2<-final[-c(6,9),]
final2 <- final %>% filter(pelvis >0)

neander_wo_djd_stat_table<-t(apply(final2,1,function(x) {
  two_rows    =   cbind(sample2,x)
  cramer_stat =   CramerV(two_rows,conf.level=0.90)
  chi2test    =   chisq.test(two_rows)
  chi2_cols   =   c(unname(chi2test$statistic), chi2test$p.value)
  frame       =   cbind(cramer_stat,chi2_cols)
  final_frame =   frame[-c(6)]
  names(final_frame) = c("cramer's v", "l.ci", "u.ci", "chi square", "p-value")
  return(final_frame)
}))

# sample without shandidar
neander_wo_shan_stat_table<-t(apply(final,1,function(x) {
  two_rows    =   cbind(sample3,x)
  cramer_stat =   CramerV(two_rows,conf.level=0.90)
  chi2test    =   chisq.test(two_rows)
  chi2_cols   =   c(unname(chi2test$statistic), chi2test$p.value)
  frame       =   cbind(cramer_stat,chi2_cols)
  final_frame =   frame[-c(6)]
  names(final_frame) = c("cramer's v", "l.ci", "u.ci", "chi square", "p-value")
  return(final_frame)
}))


# sample without shandidar or djd        

final3<-final[-c(5,6,9,14,26,54,70),]     
final3 <- final %>% filter(pelvis >0 & foot > 0)

neander_wo_djd_shan_stat_table<-t(apply(final3,1,function(x) {     
  two_rows    =   cbind(sample4,x)
  cramer_stat =   CramerV(two_rows,conf.level=0.90)
  chi2test    =   chisq.test(two_rows)
  chi2_cols   =   c(unname(chi2test$statistic), chi2test$p.value)
  frame       =   cbind(cramer_stat,chi2_cols)
  final_frame =   frame[-c(6)]
  names(final_frame) = c("cramer's v", "l.ci", "u.ci", "chi square", "p-value")
  return(final_frame)
}))


##########################################



####################################################################################################################
## create a function to cleanup the chi square tables and be able to pull out different objects from this cleanup ##
####################################################################################################################

# like chi2cleanup but with cramer's v 
chi2cleanup<-function(table){
  
  # read in one of the chi square tables (neander_total_stat_table, neander_wo_djd_stat_table, neander_wo_shan_stat_table or neander_wo_djd_shan_stat_table)
  frame<-as.data.frame(table,row.names = rownames(table))
  names(frame) = c("cramer's v", "l.ci", "u.ci", "chi square", "p-value")
  fin = na.omit(frame) 
  
  # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
  c<- list(
    rowna=frame[is.nan(frame$`chi square`),],   # $rowna 
    final=fin,                        # $final
    similar=fin[fin$`p-value` > .05,] # $similar (activities that are similar, P > 0.05)
  )
  return(c)
}


# name cleaned up chi square tables
n_tot<-chi2cleanup(neander_total_stat_table)  # neanderthal total
n_djd<-chi2cleanup(neander_wo_djd_stat_table) # neanderthal w/o djd
n_s<-chi2cleanup(neander_wo_shan_stat_table)   # neanderthal w/o shan
n_sd<-chi2cleanup(neander_wo_djd_shan_stat_table) # neanderthal w/o shan or djd

# find similar activities per sample 
n_tot$similar   # neanderthal total
n_djd$similar   # neanderthal w/o djd
n_s$similar     # neanderthal w/o shan
n_sd$similar    # neanderthal w/o shan or djd

# now what does rodeo riders look like in comparison to the neanderthal samples
n_tot$final['rodeo',]   # neanderthal total
n_djd$final['rodeo',]   # neanderthal w/o djd
n_s$final['rodeo',]     # neanderthal w/o shan
n_sd$final['rodeo',]    # neanderthal w/o shan or djd

#############################################################
## finding activities that are not statistically different ##
#############################################################
 
# create a function to find the similar activities frome the original contigency table

# like similarSelectorCounts() but returns proportions instead
similarSelector<-function(frame,final_frame,neander_sample){ 
  
  require(reshape)
  require(plyr)
  indices=rownames(frame$similar)               # find rows to set as indices
  new_rows = final_frame[indices,]              # map those to original contigency table
  
  sample =append(c("neander","rodeo"),rownames(new_rows))  # add a new vector to use a "sample" column
  
  rodeo = final_frame['rodeo',] # add rodeo riders
  
  joined_rows = rbind(neander_sample,rodeo,new_rows) # join rows from neanderthal sample to new dataframe
  
  props = prop.table(as.table(as.matrix(joined_rows)),1)
  
  props<-as.data.frame.matrix(props)
  
  joined_cols = cbind(sample,props)       # add the "sample" column
  
  rownames(joined_cols) = NULL                  # remove the row indices 
      
  melted <- melt(joined_cols, id=(c("sample"))) # transpose contigency table
  
  return(melted)                
}


# an example of how similarSelector works
simToNeanderTotal<-similarSelector(n_tot,final,sample1)
simToNeanderWOdjd<-similarSelector(n_djd,final,sample2)
simToNeanderWOShan<-similarSelector(n_s,final,sample3)
simToNeanderWOdjdOrShan<-similarSelector(n_sd,final,sample4)

# extract just the neanderthal row for emphasis in plots
n2<-simToNeanderTotal[simToNeanderTotal$sample=='neander',]
d2<-simToNeanderWOdjd[simToNeanderWOdjd$sample=='neander',]
s2<-simToNeanderWOShan[simToNeanderWOShan$sample=='neander',]
sd<-simToNeanderWOdjdOrShan[simToNeanderWOdjdOrShan$sample=='neander',]

# extract just the rodeo riders for emphasis in plots
n3<-simToNeanderTotal[simToNeanderTotal$sample=='rodeo',]
d3<-simToNeanderWOdjd[simToNeanderWOdjd$sample=='rodeo',]
s3<-simToNeanderWOShan[simToNeanderWOShan$sample=='rodeo',]
sd3<-simToNeanderWOdjdOrShan[simToNeanderWOdjdOrShan$sample=='rodeo',]
######################################################################
## plotting ##########################################################
######################################################################

# create a function to plot all of this stuff
plotMaker<-function(total_frame,neander_frame,rodeo_frame){
  ggplot(data=total_frame, # this needs to be one of the dataframes directly above
         aes(x=factor(variable), y=value, 
             group=sample,
             color=sample)) + 
    geom_line() + 
    geom_point() +
    geom_point(data=neander_frame,aes(x=factor(variable), y=value, 
                                      group=sample, size = 4))+
    geom_line(data=neander_frame,aes(x=factor(variable), y=value, 
                          group=sample, size = 2))+ 
    geom_point(data=rodeo_frame,aes(x=factor(variable), y=value, 
                                      group=sample, size = 4))+
    geom_line(data=rodeo_frame,aes(x=factor(variable), y=value, 
                                     group=sample, size = 2))+ 
    scale_x_discrete("Proportion") +
    scale_y_continuous("Body Part")+
    guides(size=FALSE)
}

plotMaker(simToNeanderTotal,n2,n3)
plotMaker(simToNeanderWOdjd,d2,d3)
plotMaker(simToNeanderWOShan,s2,s3)
plotMaker(simToNeanderWOdjdOrShan,sd,sd3)


########################################
########################################
########################################
###### OLDER ***************************
###### VERSION *************************
###### OF ******************************
###### SCRIPT **************************
########################################
########################################
########################################

# setwd(dir='~/Documents/research/neander_trauma/Desktop/Spring 2016/Neander_Trauma/')
# 
# #######################
# library(plyr)       # #
# library(ggplot2)    # #
# library(plotly)     # #
# library(reshape)    # #
# library(vcd)        # #
# library(DescTools)  # #
# #######################
# 
# # read in the data
# df <- read.csv('Data/NEISS/sport_category_final.csv')
# 
# # create a contigency table 
# orig_table <- table(df$prod1,df$body_part)
# 
# # create a datafame from the contigency table (at least one that is intuitive)
# tm<-as.data.frame.matrix(orig_table) # tm stands for table matrix
# 
# # create new columns based on body part combinations
# head_neck <- tm$face + tm$neck + tm$head
# shoulder_arm <- tm$`upper arm` + tm$`lower arm` + tm$shoulder + tm$elbow
# hand <- tm$hand + tm$finger + tm$wrist
# pelvis <- tm$`pubic region` + tm$hip 
# leg <- tm$knee + tm$`lower leg` + tm$`upper leg`
# foot <- tm$foot + tm$toe + tm$ankle
# trunk <- tm$`upper trunk` + tm$back
# 
# 
# # combine columns into a new dataframe
# final<-as.data.frame(cbind(head_neck,shoulder_arm,hand,pelvis,leg,foot,trunk))
# rownames(final)<-rownames(tm) # index will be the activity name
# 
# # recreate data from Berger Trinkaus papers
# sample1<-c(8,4,7,1,1,3,3) # total sample
# sample2<-c(7,4,7,1,0,3,1) # without djd
# sample3<-c(6,4,5,1,1,3,1) # without shandidar 1
# sample4<-c(5,4,5,1,0,3,0) # without djd or shandidar 1
# 
# ## apply chi square for neander total and every sport
# 
# # nt<-t(apply(final,1,function(x) {
# #   new<- cbind(sample1,x)
# #   ch <- chisq.test(new)
# #   c(unname(ch$statistic), ch$p.value)}))
# 
# ##############################
# ######### CRAMER'S V #########
# ##############################
# 
# ## this is an example with confidence intervals however,
# ## this was buggy and perhaps unnecessary
# 
# #tots<-t(apply(final,1,function(x) {
# #  new<- cbind(sample1,x)
# #  ch <- chisq.test(new)
# #  chi<-c(unname(ch$statistic), ch$p.value)
# #  cram<-CramerV(new,conf.level=0.90)
# #  cbind(chi,cram)
# #}))
# 
# 
# # total sample
# nt<-t(apply(final,1,function(x) {
#   new<- cbind(sample1,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample with out djd
# nwd<-t(apply(final,1,function(x) {
#   new<- cbind(sample2,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample without shandidar
# nws<-t(apply(final,1,function(x) {
#   new<- cbind(sample3,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample without shandidar or djd
# nwsd<-t(apply(final,1,function(x) {
#   new<- cbind(sample4,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # # total sample
# nt<-t(apply(final,1,function(x) {
#   new<- cbind(sample1,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample with out djd
# nwd<-t(apply(final,1,function(x) {
#   new<- cbind(sample2,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample without shandidar
# nws<-t(apply(final,1,function(x) {
#   new<- cbind(sample3,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))
# 
# # sample without shandidar or djd
# nwsd<-t(apply(final,1,function(x) {
#   new<- cbind(sample4,x)
#   ch <- chisq.test(new)
#   chi<-c(unname(ch$statistic), ch$p.value)
#   cram<-CramerV(new)
#   cbind(chi,cram)
# }))

# ####################################################################################################################
# ## create a function to cleanup the chi square tables and be able to pull out different objects from this cleanup ##
# ####################################################################################################################
# # chi2cleanup<-function(table){
# #   
# #   # read in one of the chi square tables (nt, nwd, nws or nwsd)
# #   frame<-as.data.frame(table,row.names = rownames(table))
# #   names(frame) = c('X2','P-Value')
# #   fin = na.omit(frame) 
# #   
# #   # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
# #   c<- list(
# #     rowna=frame[is.nan(frame$X2),],   # $rowna 
# #     final=fin,                        # $final
# #     similar=fin[fin$`P-Value` > .05,] # $similar (activities that are similar, P > 0.05)
# #   )
# #   return(c)
# # }
# 
# # like chi2cleanup but with cramer's v and confidence intervals
# # chi2cleanup2<-function(table){
# #   
# #   # read in one of the chi square tables (nt, nwd, nws or nwsd)
# #   frame<-as.data.frame(table,row.names = rownames(table))
# #   frame<-frame[-c(3)] # drop the extra p value column
# #   names(frame) = c('X2','P-Value','Cramers V', 'l.conf','u.conf')
# #   fin = na.omit(frame) 
# #   
# #   # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
# #   c<- list(
# #     rowna=frame[is.nan(frame$X2),],   # $rowna 
# #     final=fin,                        # $final
# #     similar=fin[fin$`P-Value` > .05,] # $similar (activities that are similar, P > 0.05)
# #   )
# #   return(c)
# # }
# 
# 
# # like chi2cleanup but with cramer's v 
# chi2cleanup<-function(table){
#   
#   # read in one of the chi square tables (nt, nwd, nws or nwsd)
#   frame<-as.data.frame(table,row.names = rownames(table))
#   frame<-frame[-c(4)] # drop the extra p value column
#   names(frame) = c('X2','P-Value','Cramers V')
#   fin = na.omit(frame) 
#   
#   # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
#   c<- list(
#     rowna=frame[is.nan(frame$X2),],   # $rowna 
#     final=fin,                        # $final
#     similar=fin[fin$`P-Value` > .05,] # $similar (activities that are similar, P > 0.05)
#   )
#   return(c)
# }
# 
# 
# # name cleaned up chi square tables
# n_tot<-chi2cleanup(nt)  # neanderthal total
# n_djd<-chi2cleanup(nwd) # neanderthal w/o djd
# n_s<-chi2cleanup(nws)   # neanderthal w/o shan
# n_sd<-chi2cleanup(nwsd) # neanderthal w/o shan or djd
# 
# # cleaned up with cramer's v
# e<-chi2cleanup2(noshan)
# 
# # find similar activities per sample 
# n_tot$similar
# n_djd$similar
# n_s$similar
# n_sd$similar
# 
# #############################################################
# ## finding activities that are not statistically different ##
# #############################################################
# 
# # create a function to find the similar activities frome the original contigency table
# similarSelectorCounts<-function(frame,final_frame,neander_sample){ 
#   
#   require(reshape)
#   
#   indices=rownames(frame$similar)               # find rows to set as indices
#   new_rows = final_frame[indices,]              # map those to original contigency table
#   
#   sample =append("neander",rownames(new_rows))  # add a new vector to use a "sample" column
#   
#   joined_rows = rbind(neander_sample,new_rows)  # join rows from neanderthal sample to new dataframe
#   joined_cols = cbind(sample,joined_rows)       # add the "sample" column
#   rownames(joined_cols) = NULL                  # remove the row indices 
#   
#   melted <- melt(joined_cols, id=(c("sample"))) # transpose contigency table
#   
#   return(melted)                
# }
# 
# # like similarSelectorCounts() but returns proportions instead
# similarSelector<-function(frame,final_frame,neander_sample){ 
#   
#   require(reshape)
#   require(plyr)
#   indices=rownames(frame$similar)               # find rows to set as indices
#   new_rows = final_frame[indices,]              # map those to original contigency table
#   
#   sample =append("neander",rownames(new_rows))  # add a new vector to use a "sample" column
#   
#   joined_rows = rbind(neander_sample,new_rows) # join rows from neanderthal sample to new dataframe
#   props = prop.table(as.table(as.matrix(joined_rows)),1)
#   
#   props<-as.data.frame.matrix(props)
#   joined_cols = cbind(sample,props)       # add the "sample" column
#   rownames(joined_cols) = NULL                  # remove the row indices 
#   
#   melted <- melt(joined_cols, id=(c("sample"))) # transpose contigency table
#   
#   return(melted)                
# }
# 
# # an example of how similarSelector works
# simToNeanderTotal<-similarSelector(n_tot,final,sample1)
# simToNeanderWOdjd<-similarSelector(n_djd,final,sample2)
# simToNeanderWOShan<-similarSelector(n_s,final,sample3)
# simToNeanderWOdjdOrShan<-similarSelector(n_sd,final,sample4)
# 
# # extract just the neanderthal row for emphasis in plots
# n2<-simToNeanderTotal[simToNeanderTotal$sample=='neander',]
# d2<-simToNeanderWOdjd[simToNeanderWOdjd$sample=='neander',]
# s2<-simToNeanderWOShan[simToNeanderWOShan$sample=='neander',]
# sd<-simToNeanderWOdjdOrShan[simToNeanderWOdjdOrShan$sample=='neander',]
# 
# ######################################################################
# ## plotting ##########################################################
# ######################################################################
# 
# # create a function to plot all of this stuff
# plotMaker<-function(total_frame,neander_frame){
#   ggplot(data=total_frame, # this needs to be one of the dataframes directly above
#          aes(x=factor(variable), y=value, 
#              group=sample,
#              color=sample)) + 
#     geom_line() + 
#     geom_point() +
#     geom_point(data=neander_frame,aes(x=factor(variable), y=value, 
#                                       group=sample, size = 4))+
#     geom_line(data=neander_frame,aes(x=factor(variable), y=value, 
#                                      group=sample, size = 2))+ 
#     scale_x_discrete("Proportion") +
#     scale_y_continuous("Body Part")+
#     guides(size=FALSE)
# }
# 
# plotMaker(simToNeanderTotal,n2)
# plotMaker(simToNeanderWOdjd,d2)
# plotMaker(simToNeanderWOShan,s2)
# plotMaker(simToNeanderWOdjdOrShan,sd)
# 
# # ggplot(data=simToNeanderWOdjd, # this needs to be one of the dataframes directly above
# #        aes(x=factor(variable), y=value, 
# #            group=sample,
# #            color=sample)) + 
# #   geom_line() + 
# #   geom_point() +
# #   geom_point(data=d2,aes(x=factor(variable), y=value, 
# #                          group=sample, size = 4))+
# #   geom_line(data=d2,aes(x=factor(variable), y=value, 
# #                         group=sample, size = 2))+ 
# #   scale_x_discrete("Proportion") +
# #   scale_y_continuous("Body Part")+
# #   guides(size=FALSE)
