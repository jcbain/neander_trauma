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

# change tables above into data frames
nt<-as.data.frame(nt,row.names = rownames(nt))
nwd<-as.data.frame(nwd,row.names=rownames(nwd))
nws<-as.data.frame(nws,row.names=rownames(nws))
nwsd<-as.data.frame(nwsd,row.names = rownames(nwsd))

# find those rows activities resulting in NaNs or the expected value within the chi square test is nothing
nt[is.nan(nt$V1),]
nwd[is.nan(nwd$V1),]
nws[is.nan(nws$V1),]
nwsd[is.nan(nwsd$V1),]


# remove NAs for final evaluation
nt<-na.omit(nt)
nwd<-na.omit(nwd)
nws<-na.omit(nws)
nwsd<-na.omit(nwsd)

names(nt)<- c('X2','P-Value')
names(nwd)<- c('X2','P-Value')
names(nws)<- c('X2','P-Value')
names(nwsd)<- c('X2','P-Value')

#  find those activities that are similar to neanderthals
nt[nt$`P-Value`>.05,]
nwd[nwd$`P-Value`>.05,]
nws[nt$`P-Value`>.05,]
nwsd[nwd$`P-Value`>.05,]

chi2cleanup<-function(table){
  # read in one of the chi square tables (nt, nwd, nws or nwsd)
  frame<-as.data.frame(table,row.names = rownames(table))
  names(frame) = c('X2','P-Value')
  
  # create two callable objects, 1) the rows that have NAs and 2) the final data frame for some final manipulation
  c<- list(
    rowna=frame[is.nan(frame$X2),],
    final=na.omit(frame)
  )
  return(c)
}

