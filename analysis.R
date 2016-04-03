setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

library(plyr)

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
  
  # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
  c<- list(
    rowna=frame[is.nan(frame$X2),], # $rowna 
    final=na.omit(frame)            # $final
  )
  return(c)
}

# name cleaned up chi square tables
first<-chi2cleanup(nt) 
second<-chi2cleanup(nwd)
third<-chi2cleanup(nws)
fourth<-chi2cleanup(nwsd)


# find those that have are similar to neanderthal samples (rename these!!!!)
first$final[first$final$`P-Value`>.05,] # for total neanderthal sample
second$final[second$final$`P-Value`>.05,] # neanderthal without djd
third$final[third$final$`P-Value`>.05,] # neanderthal without shan
fourth$final[fourth$final$`P-Value`>.05,] # neanderthal without djd or shan
