setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

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
pelvis <- tm$`pubic region` + tm$hip
leg <- tm$knee + tm$`lower leg` + tm$`upper leg`
foot <- tm$foot + tm$toe + tm$ankle
trunk <- tm$`upper trunk` + tm$back

# combine columns into a new dataframe
activity <- rownames(tm)
final<-as.data.frame(cbind(activity,head_neck,shoulder_arm,hand,pelvis,leg,foot,trunk))
