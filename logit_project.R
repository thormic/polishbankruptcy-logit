# Installing all required packages
if(!require(RWeka)){
  install.packages("RWeka")
  library(RWeka)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(caret)){
  install.packages("caret")
  library(caret)
}


# Loading dataset
df <- read.arff("data/1year.arff")


# Checking how many defaults are in the dataset
df %>% 
  filter(class == 1) %>% 
  nrow()

# Getting all the columns with a lot of NA's
columns <- df %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  Filter(function(x) { x > 0.01*nrow(df)},.) %>% 
  colnames()


# Getting all the rows with a lot of NA's
rows <- df %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0.05*ncol(df),.) %>% 
  list() %>% 
  do.call(rbind, .) %>% 
  colnames() %>% 
  as.vector()

# Subsetting dataframe without highly not available data
df1 <- df[ , -which(names(df) %in% columns)]

rows1 <- df1 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0.05*ncol(df1),.) %>% 
  list() %>% 
  do.call(rbind, .) %>% 
  colnames() %>% 
  as.vector()

df2 <- df1[-which(rownames(df1) %in% rows1),]
#---------------- Lost 93 instances (including 6 defaults)


# Creating correlation matrix
df2[,-which(names(df2)=="class")] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>%  
  corrplot(., method="circle",type = "upper")


# Choose variables which are correlatd with over 6 others with corr > 0.95
corr_vars <- df2[,-which(names(df2)=="class")] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>% 
  as.data.frame %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  filter(.,value > 0.95 & value < 1) %>% 
  plyr::count("var2") %>% 
  filter(.,freq >= 6) %>% 
  .[,1]

df3 <- df2[,-which(names(df2) %in% corr_vars)]

df3 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.)
  
df3 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0,.) %>% 
  length()







# Splitting dataset into training and testing datasets
which_train <- createDataPartition(df2$class, 
                                   p = 0.7, 
                                   list = FALSE) 
year1_train <- df2[which_train,]
year1_test <- df2[-which_train,]

