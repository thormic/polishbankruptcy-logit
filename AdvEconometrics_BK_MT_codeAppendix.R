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
if(!require(margins)){
  install.packages("margins")
  library(margins)
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


# ------------------------------ NO NA's REMAINING


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
  x[x < (qnt[1] - H_2)] <- caps[1]
  x[x < (qnt[1] - H)] <- med
  x[x > (qnt[2] + H_3)] <- NA
  x[x > (qnt[2] + H_2)] <- caps[2]
  x[x > (qnt[2] + H)] <- med
  df4[[i]] <- x
}
df5 <- df4[complete.cases(df4),]

# Histograms of all variables
ggplot(gather(df5[,-which(names(df5) %in% factor_vars)]), aes(value)) + 
  geom_histogram(bins = 100) + 
  facet_wrap(~key, scales = 'free_x') +
  ylim(0, 700)

# Checking those varaibles with a lot of zeroes
prop.table(table(df4$Attr6 == 0))
prop.table(table(df4$Attr59 == 0))
# ~ 41% of zeros in both cases

# Checking variables with near zero variance
nearZeroVar(df5,
            saveMetrics = TRUE)


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

# Releveling class so it is compatible with summarizing function
levels(year1_train$class) <- make.names(levels(factor(year1_train$class)))


###################################### Creating models
# Formula for basic model
b_formula <- class ~ .

# Formula for basic + interactions
b_i_formula <- (class ~ . + Attr2:Attr29 + Attr12_BINNED +
                  I(Attr2^2) + 
                    I(Attr25^3) +
                    I(log1p(Attr61)) +
                    I(log1p(Attr64)) +
                    Attr66:Attr12_BINNED +
                    Attr66:Attr38 +
                    Attr9:I(exp(Attr29)) + 
                    Attr2:Attr51 +
                    Attr2:Attr66 +
                    Attr2:Attr3 +
                    Attr3:Attr10 +
                    Attr3:Attr25 + 
                    Attr3:Attr38 +
                    Attr25:Attr38 +
                    Attr49:Attr56 +
                    Attr29:Attr55 - 
                    Attr69 - 
                    Attr12) 

# Formula for chosen + interactions
c_i_formula <- (class ~ Attr2 + Attr6 + Attr10 + Attr15 + Attr33 + Attr38 + Attr50 + 
                  Attr55 + Attr56 + Attr57 + Attr64 + Attr65 + Attr66 + Attr68 +
                  I(Attr2^2) + 
                  I(Attr25^3) + 
                  Attr2:Attr51 + 
                  Attr3:Attr10 + 
                  Attr3:Attr38 + 
                  Attr25:Attr38 + 
                  Attr49:Attr56 + 
                  Attr29:Attr55) 

# Formula for chosen
c_formula <- (class ~ Attr2 + Attr3 + Attr6 + Attr9 + Attr10 + Attr12 + Attr15 + Attr33 + 
                      Attr38 + Attr49 + Attr50 + Attr51 + Attr56 + Attr57 + Attr65 + 
                      Attr66 + Attr68)

# Setting Crossvalidation method with custom summary function
ctrl_cv5 <- trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 5,
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
  class <- as.factor(ifelse(probs > 0.02, lvls[2], lvls[1]))
  out <- c(rocAUC, 
           sensitivity(class, data[, "obs"], positive=positive), 
           specificity(class, data[, "obs"], negative=negative)
           )
  names(out) <- c("ROC", "Sens", "Spec")
  out
}

############################################################ MODEL 1
# Estimating forecast accuracy
year1_1 <-
  train(b_i_formula,
        data = year1_train,
        method = "glm",
        family = "binomial",
        trControl = ctrl_cv5)
year1_1


# Forecasting probabilities for test data
year1_1_forecasts <- predict(year1_1,
                                 year1_test,
                                 type = "prob")


# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_1_forecasts[,2] > 0.02, 
                                        "1",
                                        "0")), 
                reference = year1_test$class, 
                positive = "1")

# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         year1_1_forecasts[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
         year1_1_forecasts[,2])

############################################################ MODEL 2
# Next logit did not work without that command
registerDoSEQ()
set.seed(361309)

# Calculating logit with forward propagation with AIC as a cost function
# year1_2 <-
#   train(b_i_formula,
#         data = year1_train,
#         method = "glmStepAIC",
#         direction = "both",
#         trControl = ctrl_cv5)


year1_2 <- train(c_i_formula,
      data = year1_train,
      method = "glm",
      family = "binomial",
      trControl = ctrl_cv5)

year1_2


# Forecasting probabilities for test data
year1_2_forecasts <- predict(year1_2,
                               year1_test,
                               type = "prob")


# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_2_forecasts[,2] > 0.02, 
                                        1,
                                        0)), 
                reference = year1_test$class, 
                positive = "1") 


# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         year1_2_forecasts[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
         year1_2_forecasts[,2])




############################################################ MODEL 3
# Next logit did not work without that command
registerDoSEQ()
set.seed(361309)

# Calculating logit with forward propagation with AIC as a cost function
# year1_3 <-
#   train(b_formula,
#         data = year1_train,
#         method = "glmStepAIC",
#         direction = "both",
#         trControl = ctrl_cv5)

year1_3 <-
  train(c_formula,
        data = year1_train,
        method = "glm",
        family = "binomial", 
        metric = "Sens",
        trControl = ctrl_cv5,
        )
year1_3

# Forecasting probabilities for test data
year1_3_forecasts <- predict(year1_3,
                                     year1_test,
                                     type = "prob")




# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_3_forecasts[,2] > 0.02, 
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



############################################################ MODEL 3.1
# Calculating Model 3.1
year1_31 <-
  train(class ~  Attr6 + Attr12 + Attr33 + 
          Attr38 + Attr49 + Attr57 + Attr65 + 
          Attr66,
        data = year1_train,
        method = "glm",
        family = "binomial", 
        metric = "Sens",
        trControl = ctrl_cv5,
  )
year1_31

# Forecasting probabilities for test data
year1_31_forecasts <- predict(year1_31,
                             year1_test,
                             type = "prob")




# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(year1_31_forecasts[,2] > 0.02, 
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

# Odds ratio for Model 3.1
exp(year1_31$finalModel$coefficients) %>%
  as.data.frame()

# Marginal effects for Model 3.1 (works for glm object, so for it to work
# one have to simply retype the formula and create glm object instead of train)
effects_31_participation = margins(year1_31) 
summary(effects_31_participation)
plot(effects_31_participation)

# Hosmer and Lemeshow GOF test for Model 3.1
hl <- hoslem.test(year1_31$finalModel$y, year1_31$finalModel$fitted.values, g=10)
hl


#----------------------------------------------------------------------- ONE LAYER PERCEPTRON

fitControl <- trainControl(method = "repeatedcv", 
                           number = 5,
                           repeats = 5,
                           classProbs = TRUE, 
                           summaryFunction = customTwoClassSummary)

# We try to choose the best number of nodes in hidden layer and the decay value for NN
nnetGrid <-  expand.grid(size = seq(from = 60, to = 80, by = 5),
                         decay = seq(from = 0.1, to = 0.3, by = 0.1))


year1_nnet <- train(c_formula,
                 data = year1_train,
                 method = "nnet",
                 metric = "Sens",
                 trControl = ctrl_cv5,
                 tuneGrid = nnetGrid,
                 verbose = FALSE,
                 MaxNWts = 3000,
                 maxit = 350)
year1_nnet

# Predicting NN results on test data
nnet_forecasts <- predict(year1_nnet,
                                 year1_test,
                                 type = "prob")


# Confusion matrix for test data
confusionMatrix(data = as.factor(ifelse(nnet_forecasts[2] > 0.02, 
                                        1,
                                        0)), 
                reference = year1_test$class, 
                positive = "1") 

# ROC Curve for test data
roc.plot(ifelse(year1_test$class == "1", 1, 0),
         nnet_forecasts[,2])

# ROC Area for test data
roc.area(ifelse(year1_test$class == "1", 1, 0),
         nnet_forecasts[,2])



# Creating table in LaTeX but exporting it to html file
stargazer(year1_1, year1_2, year1_3, year1_31, 
          title="Results", 
          align=TRUE,
          out = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/tab2.html")



# Saving models to files (DO NOT)
# saveRDS(year_31, file = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/year1_31.rda")
# saveRDS(year1_1, file = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/year1_1.rda")
# saveRDS(year1_2, file = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/year1_2.rda")
# saveRDS(year1_3, file = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/year1_3.rda")
# saveRDS(nnetFit, file = "C:/Users/Michał/GIT/polishBankruptcyPredictionLogit/nnetFit_2.rda")
