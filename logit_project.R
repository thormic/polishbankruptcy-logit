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
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(mice)){
  install.packages("mice")
  library(mice)
}
if(!require(VIM)){
  install.packages("VIM")
  library(VIM)
}
if(!require(verification)){
  install.packages("verification")
  library(verification)
}


# Setting seed for reproduciblity
set.seed(361309)


# Loading dataset
df <- read.arff("data/1year.arff")


# Checking how many defaults are in the dataset
df %>% 
  filter(class == 1) %>% 
  nrow()


# Checking how many NA's there are for each variable
df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.) %>% 
  sort(decreasing = TRUE)


# Checking how many rows are with NA's
df %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0,.) %>% 
  sort(decreasing = TRUE)


# Plotting NA's
aggr(df, 
     numbers = TRUE, 
     prop = FALSE,
     sortVars = TRUE, # sort variables by # of missings
     cex.axis = .5, # decrease the labels on axes 
     gap = 2, # limit the gap between plots (default = 4)
     ylab = c("Number of misisngs", "pattern")) # labels for y


# Setting factor variables for missing data 
# (65 - no long-term liabilities, 66 - no financial expenses, 67 - no inventory(pokrywa 60 i 45), 
# 68 - no extraordinary items or no financial expenses, 69 - no receivables)
df$Attr65 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr37'])) 1 else 0))
df$Attr66 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr27'])) 1 else 0))
df$Attr67 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr60'])) 1 else 0))
df$Attr68 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr11'])) 1 else 0))
df$Attr69 <- as.factor(apply(df, 1, FUN = function(x) if (is.na(x['Attr61'])) 1 else 0))



# Creating list of factor variables
factor_vars <- c("class", "Attr65", "Attr66", "Attr67", "Attr68", "Attr69")

# Getting all the columns with a lot of NA's (over 3% of observations are NA) 
df %>%
  dplyr::select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  Filter(function(x) { x > 0.03*nrow(df)},.) %>% 
  colnames() 

na_columns <- c("Attr21", "Attr27", "Attr37", "Attr60", "Attr45", "Attr24")

# Subsetting dataframe without highly not available data
df1 <- df[ , -which(names(df) %in% na_columns)]

df1 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.) %>% 
  sort(decreasing = TRUE)


# Most of NA's within observations came from the same missings of certain data - so we omit these observations - there were almsot no defaults 
# so we do not lose any major data
omit_cols <- c("Attr16", "Attr26", "Attr41", "Attr32", "Attr47", "Attr52", "Attr28", "Attr53", "Attr54", "Attr64", "Attr46", "Attr12", "Attr4",
               "Attr33", "Attr40", "Attr63")



df2 <- df1[complete.cases(df1[, omit_cols]), ]
# Retained ~3.84% level of defaults in dataset


# Left one obs with 19 NA's and a lot of obs with 1 NA 

rownames(df2) <- NULL

# Getting all the rows with over 2 NA values - only one observation in our case
na_rows <- df2 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x >= 2,.) %>% 
  list() %>% 
  do.call(rbind, .) %>% 
  colnames() %>% 
  as.vector()


# Replacing NA's in 11 and 61 with median (having a binary variable indicating that these were missing)
df2 <- df2 %>% 
  mutate(Attr61 = ifelse(is.na(Attr61),
                         median(Attr61, na.rm = TRUE),
                         Attr61),
         Attr11 = ifelse(is.na(Attr11), 
                         median(Attr11, na.rm = TRUE),
                         Attr11)
  ) 


# Omitting rows containing a lot of NA's
df3 <- df2[-which(rownames(df2) %in% na_rows),]






# Creating correlation matrix
df3[,-which(names(df3) %in% factor_vars)] %>%
  as.matrix() %>% 
  cor() %>%  
  corrplot(., method="circle",type = "upper")


# Choosing variables which are correlated with over 6 others with corr > 0.95
df3[,-which(names(df3) %in% factor_vars)] %>%
  na.omit() %>% 
  as.matrix() %>% 
  cor() %>% 
  as.data.frame %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  filter(.,value > 0.9 & value < 1) %>% 
  plyr::count("var2") %>%
  filter(.,freq >= 2) %>%
  .[,1]


corr_columns <- c("Attr1", "Attr11", "Attr13", "Attr14", "Attr16", "Attr17", "Attr18", "Attr19", 
                  "Attr20", "Attr22", "Attr23", "Attr26", "Attr28", "Attr30", "Attr31", "Attr32", 
                  "Attr35", "Attr39", "Attr4", "Attr40", "Attr42", "Attr43", "Attr44", "Attr46", 
                  "Attr47", "Attr52", "Attr53", "Attr54", "Attr58", "Attr62", "Attr63", "Attr7", "Attr8")

# Omitting highly correlated variables
df4 <- df3[,-which(names(df3) %in% corr_columns)]


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


# Creating correlation matrix for final dataset
df4[,-which(names(df4) %in% factor_vars)] %>%
  as.matrix() %>% 
  cor() %>%  
  corrplot(., method="circle",type = "upper")

str(df4)

summary(df4)
# df4$class <- as.numeric(df4$class)
# df4$class <- apply(df4, 1, FUN = function(x) if (x['class'] == '2') 1 else 0)



# Removing/changing values of outliers
for (i in names(df4[,-which(names(df4) %in% factor_vars)])) {
  x <- df4[[i]]
  med <- quantile(x, probs=0.5, na.rm = T)
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  H_2 <- 3 * IQR(x, na.rm = T)
  H_3 <- 4.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H_3)] <- NA
  x[x < (qnt[1] - H_2)] <- med
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H_3)] <- NA
  x[x > (qnt[2] + H_2)] <- med
  x[x > (qnt[2] + H)] <- caps[2]
  df4[[i]] <- x
}
df5 <- df4[complete.cases(df4),]

# Histograms of all variables
ggplot(gather(df5[,-which(names(df5) %in% factor_vars)]), aes(value)) + 
  geom_histogram(bins = 100) + 
  facet_wrap(~key, scales = 'free_x')


# Checking those varaibles with a lot of zeroes
prop.table(table(df4$Attr6 == 0))
prop.table(table(df4$Attr59 == 0))
# 41% of zeros in both cases

nearZeroVar(df5,
            saveMetrics = TRUE)
# Incentive to remove Attr69 from further analysis - no variance and constant values of 0 - same for Attr6 and Attr59


# Outliers boxplot
m1 <- melt(df5[,-which(names(df5) %in% factor_vars)])
p <- ggplot(m1, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")


# Splitting dataset into training and testing datasets
which_train <- createDataPartition(df5$class, 
                                   p = 0.7, 
                                   list = FALSE) 
year1_train <- df5[which_train,]
year1_test <- df5[-which_train,]


# Creating model

model_formula <- class ~ . - Attr68 -1

ctrl_cv5 <- trainControl(method = "cv",
                         number = 5)

year1_logit_forward <- 
  train(model_formula,
        data = year1_train,
        method = "glmStepAIC",
        direction = "forward", 
        trControl = ctrl_cv5)

summary(year1_logit_forward)


# Predicting values on training set
year1_logit_fitted <- predict(year1_logit_forward,
                                        year1_train,
                                        type = "prob")

head(year1_logit_fitted)


# Confusion matrix for train data
confusionMatrix(data = as.factor(ifelse(year1_logit_fitted["1"] > 0.05, 
                                        1,
                                        0)), 
                reference = year1_train$class, 
                positive = "1") 


# Forecasting probabilities for test data
year1_logit_forecasts <- predict(year1_logit_forward,
                                           year1_test,
                                           type = "prob")




# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_logit_forecasts["1"] > 0.05, 
                                        1,
                                        0)), 
                reference = year1_test$class, 
                positive = "1") 


# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         year1_predicted[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
                year1_predicted[,2])




s <- c("Attr2", "Attr3", "Attr5", "Attr6", "Attr9", "Attr10", "Attr12", "Attr15", 
       "Attr25", "Attr29", "Attr33", "Attr34", "Attr36", "Attr38", "Attr41", "Attr48", 
       "Attr49", "Attr50", "Attr51", "Attr55", "Attr56", "Attr57", "Attr59", "Attr61", 
       "Attr64", "Attr65",  "Attr67", "Attr68", "Attr66", "Attr69")




#----------------------------------------------------------------------- LASSO 



parameters_lasso <- expand.grid(alpha = c(0,1),
                                lambda = seq(0, 0.30, 0.01))

year1_lasso <- train(model_formula, 
                     data = year1_train,
                     method = "glmnet", 
                     family="binomial",
                     tuneGrid = parameters_lasso,
                     trControl = ctrl_cv5)

year1_lasso

plot(year1_lasso)

# what is the best lambda value (giving the lowest error
# forecasts based on cross validation)?

year1_lasso$bestTune$lambda

# very close to 0 (similar to OLS)

# zobaczmy współczynniki dla zmiennych w tym modelu 

predict(year1_lasso$finalModel, # stored model
        s = year1_lasso$bestTune$lambda, # lambda
        type = "coefficients")

year1_lasso_model <- year1_lasso$finalModel

