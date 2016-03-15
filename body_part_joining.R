setwd(dir='Desktop/Spring 2016/Neander_Trauma/')

# read in the data
df <- read.csv('Data/NEISS/sport_category_final.csv')

# create a contigency table 
orig_table <- table(df$prod1,df$body_part)

# create a datafame from the contigency table (at least one that is intuitive)
tableMatrix<-as.data.frame.matrix(orig_table)

