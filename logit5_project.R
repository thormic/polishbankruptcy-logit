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
if(!require(Information)){
  install.packages("Information")
  library(Information)
}
if(!require(smbinning)){
  install.packages("smbinning")
  library(smbinning)
}
if(!require(DescTools)){
  install.packages("DescTools")
  library(DescTools)
}
if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}
if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)
}
if(!require(ResourceSelection)){
  install.packages("ResourceSelection")
  library(ResourceSelection)
}


# Setting seed for reproduciblity
set.seed(361309)


# Loading dataset
df <- read.arff("data/5year.arff")


# Checking how many defaults are in the dataset
df %>% 
  filter(class == 1) %>% 
  nrow()


# Checking how many NA's there are for each variable
most_nas <- df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  Filter(function(x) { x > 0},.) %>% 
  sort(decreasing = TRUE) %>% 
  .[1:5] %>% 
  colnames(.)


# Checking how many rows are with NA's
df %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x > 0,.) %>% 
  sort(decreasing = TRUE) %>% 
  length()


# Plotting NA's
aggr(df[,which(colnames(df) %in% most_nas)], 
     numbers = TRUE, 
     prop = FALSE,
     sortVars = TRUE, # sort variables by # of missings
     cex.axis = .5, # decrease the labels on axes 
     gap = 3, # limit the gap between plots (default = 4)
     ylab = c("Number of misisng", "Pattern")) # labels for y


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

# Adding most of the columns which we omit because factor variables above
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



rownames(df2) <- NULL

na_rows <- df2 %>% 
  rowwise() %>% 
  is.na() %>% 
  rowSums() %>% 
  Filter(function(x) x >= 1,.) %>% 
  list() %>% 
  do.call(rbind, .) %>% 
  colnames() %>% 
  as.vector()


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


# Choosing variables which are correlated with over 2 others with corr > 0.9
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


corr_columns <- c("Attr1", "Attr10", "Attr12", "Attr14", "Attr16", "Attr17", "Attr18", "Attr19", 
                  "Attr2", "Attr23", "Attr25", "Attr26", "Attr28", "Attr31", "Attr33", 
                  "Attr34", "Attr38", "Attr4", "Attr40", "Attr46", 
                  "Attr50", "Attr53", "Attr54", "Attr63", "Attr64","Attr7", "Attr8")

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
  H_2 <- 5 * IQR(x, na.rm = T)
  H_3 <- 10 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H_3)] <- NA
  x[x < (qnt[1] - H_2)] <- caps[1]
  x[x < (qnt[1] - H)] <- med
  x[x > (qnt[2] + H_3)] <- NA
  x[x > (qnt[2] + H_2)] <- caps[2]
  x[x > (qnt[2] + H)] <- med
  df4[[i]] <- x
}
df5 <- df4[complete.cases(df4),]

df5 %>% 
  filter(class == 1) %>% 
  nrow() %>% 
  '/'(nrow(df5))

# Histograms of all variables
ggplot(gather(df5[,-which(names(df5) %in% factor_vars)]), aes(value)) + 
  geom_histogram(bins = 100) + 
  facet_wrap(~key, scales = 'free_x') +
  ylim(0, 700)


# Checking those varaibles with a lot of zeroes
prop.table(table(df4$Attr6 == 0))
prop.table(table(df4$Attr59 == 0))
# ~ 41% of zeros in both cases

nearZeroVar(df5,
            saveMetrics = TRUE)
# Incentive to remove Attr69 from further analysis - no variance and constant values of 0 - same for Attr6 and Attr59


# Outliers boxplot
m1 <- melt(df5[,-which(names(df5) %in% factor_vars)])
p <- ggplot(m1, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")


# Variables transformation - binning Attr12
Attr12_binned = smbinning.custom(df = data.frame(df5 %>%
                                                   mutate(class_1 =
                                                            ifelse(class == 1,
                                                                   1,
                                                                   0))),
                                 y = "class_1",
                                 x = "Attr12",
                                 cuts = 0.06
)
Attr12_binned$ivtable
Attr12_binned$ivtable$WoE

df5 <- 
  smbinning.gen(df5,
                Attr12_binned,
                "Attr12_BINNED")
levels(df5$Attr12_BINNED) <- 
  Attr12_binned$ivtable$WoE[1:nlevels(df5$Attr12_BINNED)]

df5$Attr12_BINNED <-
  as.numeric(levels(df5$Attr12_BINNED))[df5$Attr12_BINNED]




# Splitting dataset into training and testing datasets
set.seed(361309)
which_train <- createDataPartition(df5$class,
                                   p = 0.7,
                                   list = FALSE)
year1_train <- df5[which_train,]
year1_test <- df5[-which_train,]

levels(year1_train$class) <- make.names(levels(factor(year1_train$class)))


ctrl_cv5 <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = function(...) customTwoClassSummary(..., 
                                                                               positive = "X1", negative="X0"), 
                         classProbs = TRUE)

customTwoClassSummary <- function(data, lev = NULL, model = NULL, positive = NULL, negative=NULL) 
{
  lvls <- levels(data$obs)
  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  caret:::requireNamespaceQuietStop("ModelMetrics")
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                     1), data[, lvls[1]])
  probs <- data[,lvls[2]]
  class <- as.factor(ifelse(probs > 0.03, lvls[2], lvls[1]))
  out <- c(rocAUC, 
           sensitivity(class, data[, "obs"], positive=positive), 
           specificity(class, data[, "obs"], negative=negative)
  )
  names(out) <- c("ROC", "Sens", "Spec")
  out
}

# Next logit did not work without that command
registerDoSEQ()
set.seed(361309)

# Calculating logit with forward propagation with AIC as a cost function

year1_3 <-
  train(class ~ (. -
                   Attr68 -
                   Attr69),
        data = year1_train,
        method = "glmStepAIC",
        direction = "backward",
        trControl = ctrl_cv5)

year1_3 <-
  train(class ~ Attr3 + Attr6 + Attr13 + Attr22 + Attr29 + Attr30 + Attr32 + 
          Attr39 + Attr41 + Attr42 + Attr44 + Attr48 + Attr51 + Attr52 + 
          Attr55 + Attr62 + Attr66 + Attr67,
        data = year1_train,
        method = "glm",
        family = "binomial", 
        metric = "Sens",
        trControl = ctrl_cv5,
  )
year1_3
summary(year1_3)

# Forecasting probabilities for test data
year1_3_forecasts <- predict(year1_3,
                              year1_test,
                              type = "prob")

# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_3_forecasts[,2] > 0.04, 
                                        1,
                                        0)), 
                reference = year1_test$class, 
                positive = "1") 


# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         year1_3_forecasts[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
         year1_3_forecasts[,2])


################################################

year1_31 <-
  train(class ~ Attr3 + Attr13  + Attr29 + Attr30 + Attr32 + 
          Attr39 + Attr41 + Attr44 + Attr52 + 
         Attr62 + Attr66,
        data = year1_train,
        method = "glm",
        family = "binomial", 
        metric = "Sens",
        trControl = ctrl_cv5,
  )
year1_31
summary(year1_31)

year1_31 <- glm(class ~ Attr3 + Attr13 + Attr29 + Attr30 + Attr32 + 
  Attr39 + Attr41 + Attr44 + Attr52 + 
  Attr62 + Attr66,
data = year1_train,
family = "binomial", 
) 

# Forecasting probabilities for test data
year1_31_forecasts <- predict(year1_31,
                             year1_test,
                             type = "prob")




# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_31_forecasts[,2] > 0.03, 
                                        1,
                                        0)), 
                reference = year1_test$class, 
                positive = "1") 


# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         year1_31_forecasts[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
         year1_31_forecasts[,2])

exp(year1_31$finalModel$coefficients) %>% 
  as.data.frame()

# Marginal effects for Model 3.1
effects_logit_participation = margins(year1_31) 
summary(effects_logit_participation)
plot(effects_logit_participation)


