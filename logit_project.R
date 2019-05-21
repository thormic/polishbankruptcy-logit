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
if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}
if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}
if(!require(Rpdb)){
  install.packages("Rpdb")
  library(Rpdb)
}

# Loading dataset
df <- read.arff("data/1year.arff")


# Checking how many defaults are in the dataset
df %>% 
  filter(class == 1) %>% 
  nrow()


# Checking how many NA's there are for each variable
df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.)


# Checking how many rows are with NA's
df %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0,.) %>% 
  length()

# Setting factor variables for missing data 
# (65 - no long-term liabilities, 66 - no financial expenses, 67 - no inventory)
df$Attr65 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr37'])) 1 else 0))
df$Attr66 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr27'])) 1 else 0))
df$Attr67 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr60'])) 1 else 0))

# Getting all the columns with a lot of NA's (over 1% of observations are NA)
na_columns <- df %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  Filter(function(x) { x > 0.01*nrow(df)},.) %>% 
  colnames()


# Subsetting dataframe without highly not available data
df1 <- df[ , -which(names(df) %in% na_columns)]

# Getting all the rows with over 2 NA values (including 6 defaults - 
# they make for ~6% of deleted rows so the proportion is remained considering whole dataset for which default ratio is ~4%)
na_rows <- df1 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 2,.) %>% 
  list() %>% 
  do.call(rbind, .) %>% 
  colnames() %>% 
  as.vector()

# Omitting rows containing a lot of NA's
df2 <- df1[-which(rownames(df1) %in% na_rows),]


# Creating correlation matrix
df2[,-which(names(df2) %in% c("class", "Attr65", "Attr66", "Attr67"))] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>%  
  corrplot(., method="circle",type = "upper")


# Choosing variables which are correlated with over 6 others with corr > 0.95
corr_columns <- df2[,-which(names(df2) %in% c("class", "Attr65", "Attr66", "Attr67"))] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>% 
  as.data.frame %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  filter(.,value > 0.95 & value < 1 | value < -0.95) %>% 
  plyr::count("var2") %>%
  filter(.,freq >= 1) %>%
  .[,1]

# Omitting highly correlated variables
df3 <- df2[,-which(names(df2) %in% corr_columns)]



# Omitting variables 32 and 46 because they have a lot of NA's and they are highly correlated to 52 and 40 respectively
df4 <- df3


# Omitting 4 observations, because there were NA's which were impossible to fill - not connected with any other variable in any way
# and there were no defaults omitted.
df4 <- df4[-which(is.na(df4["Attr61"]) | is.na(df4["Attr5"])),]


df4 <- df4[sample(nrow(df4)),]


# Resetting index
rownames(df4) <- NULL


# Checking NA's in dataset - where are the remaining ones occuring
df4 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.)

df4 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0,.) %>% 
  length()


# ------------------------------ NO NA's REMAINING, HURRAY!
df4[,-which(names(df4) %in% c("class", "Attr65", "Attr66", "Attr67"))] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>%  
  corrplot(., method="circle",type = "upper")
str(df4)
summary(df4)
# df4$class <- as.numeric(df4$class)
# df4$class <- apply(df4, 1, FUN = function(x) if (x['class'] == '2') 1 else 0)

# Splitting dataset into training and testing datasets
which_train <- createDataPartition(df4$class, 
                                   p = 0.7, 
                                   list = FALSE) 
year1_train <- df4[which_train,]
year1_test <- df4[-which_train,]

year1_logit1 <- glm(class ~ Attr66 + Attr12 + Attr25 + Attr48 + Attr47 + Attr67 + 
                      Attr15,
                       # here we define type of the model
                       family =  binomial,
                       data = year1_train,
                       maxit = 1000)

year1_logit2 <- glm(class ~ .,
                    # here we define type of the model
                    family =  binomial,
                    data = year1_train,
                    maxit = 1000)


summary(year1_logit2)

year1_predicted <- predict(year1_logit1,
                           type = "response")

head(year1_predicted)

( ctable <- confusionMatrix(data = as.factor(ifelse(year1_predicted > 0.024, "1", "0")), 
reference = as.factor(year1_train$class), 
positive = "1") 
)


model_formula <- class ~ . - Attr65 - 1 

ctrl_nocv <- trainControl(method = "none")

year1_logit_forward <- 
  train(model_formula,
        data = year1_train, 
        # stepwise method
        method = "glmStepAIC",
        # additional argument
        direction = "forward", 
        trControl = ctrl_nocv)


roc.plot(ifelse(year1_train$class == "1", 1, 0),
         year1_predicted)


# Sprawdzić outliery

library(tidyr)
library(ggplot2)

ggplot(gather(year1_train[,-which(names(year1_train) %in% c("class", "Attr65", "Attr66", "Attr67", "Attr68"))]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

outliers <- boxplot.stats(year1_train$Attr5)$out
boxplot(year1_train$Attr5, boxwex = 0.1)
mtext(paste("Outliers:", paste(outliers, collapes=", ")), cex=0.6)


for (i in names(year1_train[,-which(names(year1_train) %in% c("class", "Attr65", "Attr66", "Attr67", "Attr68"))])) {
  x <- year1_train[[i]]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  H_2 <- 1 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x < (qnt[1] - H_2)] <- caps[1]
  x[x > (qnt[2] + H)] <- NA
  x[x > (qnt[2] + H_2)] <- caps[2]
  year1_train[[i]] <- x
}
