# Setting up the working directory and loading the files

setwd("C:/Users/mishrsat/Desktop/Automation/codes/Mckinsey_Hack")
train <- read.csv("train.csv", header = TRUE, sep = ",", na.strings = c(""," ","NA"))
test <- read.csv("test.csv", header = TRUE, sep = ",", na.strings = c(""," ","NA"))

# Knowing the variables in train dataset.

# id - Unique ID of the policy
# perc_premium_paid_by_cash_credit - Percentage of premium amount paid by cash or credit card
# age_in_days - Age in days of policy holder
# Income - Monthly Income of policy holder
# Count_3-6_months_late - No of premiums late by 3 to 6 months
# Count_6-12_months_late - No  of premiums late by 6 to 12 months
# Count_more_than_12_months_late - No of premiums late by more than 12 months
# application_underwriting_score - Underwriting Score of the applicant at the time of application (No applications under the # score of 90 are insured)
# no_of_premiums_paid - Total premiums paid on time till now
# sourcing_channel - Sourcing channel for application
# residence_area_type - Area type of Residence (Urban/Rural)
# premium - Monthly premium amount
# renewal - Policy Renewed? (0 - not renewed, 1 - renewed

#--------------------SANITY CHECK----------------------#
# Checking for duplicate values
dup <- duplicated(train$id)
sum(dup)
# There are no duplicate IDs

# Lets look at the structure of each variable
str(train)

# Other than sourcing_channel & residence_area_type which are factor variables
# all other are either numeric or integer.

# Now lets check the number of NA's in each variable of train set.
sapply(train, function(x) sum(is.na(x)))

#--------------MISSING VALUE TREATMENT----------------------#

# There are NA values in the below variables.
# Count_3.6_months_late - 97 NA's
# Count_6.12_months_late - 97 NA's
# Count_more_than_12_months_late - 97 NA's
# application_underwriting_score - 2974 NA's

# Lets check the missing values in each of the above variables taking Count_3.6_months_late  
Count_3.6_months_late_missing <- which(is.na(train$Count_3.6_months_late))
View(train[Count_3.6_months_late_missing, ])
length(Count_3.6_months_late_missing) # 97

# There are 97 missing values in Count_3.6_months_late, Count_6.12_months_late 
# & Count_more_than_12_months_late which contributes as low as 0.00121 % of total observations
# removing them will not impact the models we create. Hence lets remove them.

train <- train[-which(is.na(train$Count_3.6_months_late)),]

# we are left out with application_underwriting_score as of now on missing values.
# Rechecking for NA values.
sapply(train, function(x) sum(is.na(x)))

# There are 2901 NA values in application_underwriting_score which is 3 % of total observations  
# We don't know what is the best way of treating NAs here, so let them be NA for now.

#-----------------------------------------------------#

# Lets look at the information value attached to each variable

# Information value is a useful technique to select important variables in a predictive model.
# It helps to rank variables on the basis of their importance.

library(ggplot2)
library(Information)

IV <- create_infotables(data = train, y = "renewal", ncore = 2)

# Creating a dataframe containing IV values of all the variables
IV_dataframe <- IV$Summary
str(IV_dataframe)

#Summary
#Variable           IV
#2  perc_premium_paid_by_cash_credit 8.672457e-01
#6            Count_6.12_months_late 6.900339e-01
#5             Count_3.6_months_late 5.434742e-01
#7    Count_more_than_12_months_late 4.555001e-01
#3                       age_in_days 1.819265e-01
#8    application_underwriting_score 9.546324e-02
#4                            Income 7.879804e-02
#9               no_of_premiums_paid 7.476009e-02
#12                          premium 3.585894e-02
#10                 sourcing_channel 2.994478e-02
#1                                id 1.912751e-03
#11              residence_area_type 8.610442e-05

# The rule of thumb for Information value states that
# A variable is useless if IV is < 0.02
# A variable is weak if IV is [0.02, 0.1)
# A variable is medium if IV is [0.1, 0.3)
# A variable is strong if IV is [0.3, 0.5)
# Anything after this is suspicious.

#-----------------------------------------------------#

# Creating a UDF for the above purpose.
for(i in 1:nrow(IV_dataframe)){
  
  if (IV_dataframe$IV[i]<0.02){
    IV_dataframe$feedback[i] = "Useless"
    
  } else if(IV_dataframe$IV[i]>=0.02& IV_dataframe$IV[i]<0.1){
    IV_dataframe$feedback[i] = "Weak"
    
  } else if(IV_dataframe$IV[i]>=0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Medium"
    
  }else if(IV_dataframe$IV[i]>=0.3 & IV_dataframe$IV[i]<0.5){
    IV_dataframe$feedback[i] = "Strong"
    
  }else if(IV_dataframe$IV[i]>0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_dataframe)
IV_dataframe$Variable <- as.factor(IV_dataframe$Variable)
IV_dataframe$feedback <- as.factor(IV_dataframe$feedback)

#-----------------------------------------------------#

# Based on feedback let extract the STRONG & MEDIUM variables
imp_vars <- which(IV_dataframe$feedback=="Strong"|IV_dataframe$feedback=="Medium")
k1 <- IV_dataframe[imp_vars, 1] # "Count_more_than_12_months_late" , "age_in_days" 

# Storing the variables "Count_more_than_12_months_late" , "age_in_days" as a matrix
imp <- which(colnames(train) %in% k1)
str(train)

# Creating a new dataset storing only the required variables.
train_new <- train[, c(1, imp, 13)]

#-----------------------------------------------------#

# Now lets create a new information value to re-check.
info_val_new <- create_infotables(train_new[, -1], y="renewal", ncore = 2)
info_val_new$Summary
info_val_new$Tables

knitr::kable(head(info_val_new$Summary))
#-----------------------------------------------------#
#|   |Variable                       |        IV|
#  |:--|:------------------------------|---------:|
#  |2  |Count_more_than_12_months_late | 0.4555001|
#  |1  |age_in_days                    | 0.1819265|

#-----------------------------------------------------#

# Woe Analysis
# The most crucial variables seem to be:
# Count_more_than_12_months_late
# age_in_days

# These should appear in the model unless they are not collinear
#-----------------------------------------------------#
#               FEATURE ENGINEERING
#-----------------------------------------------------#

#-----------------------------------------------------#
#             VARIABLE TRANSFORMATION
#-----------------------------------------------------#
# We will leave the ID variable from our analysis as its
# just a unique identifier

# Calling the required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(car)
library(Hmisc)
library(ROCR)
library(DMwR)
library(GGally)
library(ranger)
library(MASS)
library(caTools)
library(randomForest)
library(caret)

#-----------------------------------------------------#
# As we move into variable transformation we should understand WOE.

# For the record, weight of evidence (WOE) tells the predictive power of an
# independent variable in relation to the dependent variable. WOE should always
# be monotonic, which means either they should increase or decrease with the bins.

# Woe is a measure of how well a variable separates the good customers from the bad ones.

# WOE = log(local_good/total_good) - log(local_bad/total_bad)
# OR
# WOE = ln(Distribution of good/Distribution of Bads)

#-----------------------------------------------------#
# Creating a train_woe file where the actual values will
# be replaced with woe values

train_woe <- train
str(train_woe)

info_val <- create_infotables(train_woe[, -1], y="renewal", ncore = 2)


# Let's replace the variables with woe value for model building 
woe_list <- list()

typeof(info_val$Tables)
length(info_val$Tables)

# Extracting only the bins and the woe values of each bin
for(i in 1:length(info_val$Tables)) {
  woe_list[[i]] = cbind(info_val$Tables[[i]][1],info_val$Tables[[i]][4])
}

woe_list

# woe_list is a list of dataframes with each dataframe
# having the below structure:
woe_list[[1]]
str(woe_list[[1]])

#-----------------------------------------------------#

# We now need to replace the values in the train_woe dataframe
# with the corresponding woe values.

# Lets create a function which takes in the woe_list and a column
# of train_woe and replaces the actual values by woe values

# Creating the UDF
woe_function <- function(df, variable) {
  for(i in 1:nrow(df)){
    s <- df[i,1]
    if(s=="NA"){
      replace_by = df[i,2]
      variable[which(is.na(variable))] = replace_by
    } else {
      s <- str_replace_all(s, fixed(" "), "")
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      replace_by = df[i,2]
      
      variable[which(variable %in% range)] = replace_by
    }
  }
  return(variable)
}

#-----------------------------------------------------#

# Creating a empty matrix to store the variables later
empty_matrix <- matrix(0, nrow(train_woe), ncol(train_woe))

# This vector will help us in replacing the original values with WOE values
# exclusding id, renewal, application_underwriting_score as it has NAs and
# sourcing_channel, residence_area_type as they are categorical variables.
col_replace <- which(!colnames(train_woe) %in% c("id","sourcing_channel", "application_underwriting_score",
                                                 "residence_area_type","renewal"))

# Replacing 8 column data with WOE values.
for(i in col_replace){
  train_woe[, i] = woe_function(df=woe_list[[i-1]], variable = train_woe[,i])  
}

# The remaining variables needs to be done manually.
# application_underwriting_score
# sourcing_channel
# residence_area_type

#-----------------------------------------------------#

# Variable - sourcing_channel

str(train_woe$sourcing_channel)
# Factor w/ 5 levels "A","B","C","D",..: 3 1 3 1 2 2 2 1 1 1 ...

# Below is the woe values for sourcing channel
#sourcing_channel         WOE
#1                A  0.15189824
#2                B -0.03663604
#3                C -0.19501574
#4                D -0.32265505
#5                E -0.21345988

# Storing the levels
levels(train_woe$sourcing_channel) <-c(0.15189824,-0.03663604,-0.19501574,-0.32265505,-0.21345988)

# Changing to WOE values
train_woe$sourcing_channel <- as.numeric(levels(train_woe$sourcing_channel))[train_woe$sourcing_channel]

#-----------------------------------------------------#

# Variable - residence_area_type

str(train_woe$residence_area_type)
# Factor w/ 2 levels "Rural","Urban": 2 2 1 2 2 1 2 2 2 1 ...

# Below is the woe values for residence_area_type
#residence_area_type         WOE
#1               Rural -0.01139561
#2               Urban  0.00755598

# Storing the levels
levels(train_woe$residence_area_type) <-c(-0.01139561,0.00755598)

# Changing to WOE values
train_woe$residence_area_type <- as.numeric(levels(train_woe$residence_area_type))[train_woe$residence_area_type]

#-----------------------------------------------------#

# Variable - application_underwriting_score

str(train_woe$application_underwriting_score)

# Below is the woe values for application_underwriting_score
#application_underwriting_score         WOE
#1                              NA  0.13611435
#2                    [91.9,98.28] -0.63557309
#3                   [98.29,98.68] -0.34552064
#4                    [98.69,98.9] -0.07030956
#5                   [98.91,99.06]  0.05491282
#6                    [99.07,99.2]  0.22795486
#7                   [99.21,99.33]  0.32268251
#8                   [99.34,99.46]  0.23130161
#9                    [99.47,99.6]  0.13166308
#10                  [99.61,99.78]  0.09351728
#11                  [99.79,99.89]  0.32724032

train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 91.9 & 
                                                 train_woe$application_underwriting_score <=98.28)] <- '-0.63557309'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 98.29 & 
                                                 train_woe$application_underwriting_score <=98.68)] <- '-0.34552064'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 98.69 & 
                                                 train_woe$application_underwriting_score <=98.9)] <- '-0.07030956'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 98.91 & 
                                                 train_woe$application_underwriting_score <=99.06)] <- '0.05491282'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.07 & 
                                                 train_woe$application_underwriting_score <=99.2)] <- '0.22795486'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.21 & 
                                                 train_woe$application_underwriting_score <=99.33)] <- '0.32268251'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.34 & 
                                                 train_woe$application_underwriting_score <=99.46)] <- '0.23130161'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.47 & 
                                                 train_woe$application_underwriting_score <=99.6)] <- '0.13166308'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.61 & 
                                                 train_woe$application_underwriting_score <=99.78)] <- '0.09351728'
train_woe$application_underwriting_score[which(train_woe$application_underwriting_score >= 99.79 & 
                                                 train_woe$application_underwriting_score <=99.89)] <- '0.32724032'

#-----------------------------------------------------#

# NA's are just 3 % in the entire dataset, which does not 
# breach the standard threshold of 5 % hence we will remove them.

train_woe <- na.omit(train_woe)
str(train_woe)

# Changing application_underwriting_score to numeric
train_woe$application_underwriting_score <- as.numeric(train_woe$application_underwriting_score)

#-----------------------------------------------------#

# Is the dependant variable balanced, lets check
sum(train_woe$renewal)*100/nrow(train_woe)
# if you see the fail to pay rate in train_woe dataset, it shows 6.22%
# which clearly indicates that the dataset contains very larger number
# of good customers than the bad ones. Thus, for building the model on
# such kind of data requires some way of balancing the data.

#-----------------------------------------------------#
#       MODEL BUILDING LoGISTIC REGRESSION
#-----------------------------------------------------#

# Creating a new dataframe
train_woe_new <- train_woe[,-1]

# Data preparation for model building
train_woe_new$renewal <- as.factor(train_woe_new$renewal)

# Setting up the seed and spliting the data
set.seed(1000)
split_indices <- sample.split(train_woe_new$renewal, SplitRatio = 0.70)

# Train data
train_model1 <- train_woe_new[split_indices, ]

# Sample test data
test_model1 <- train_woe_new[!split_indices, ]

# Since the data is highly imbalanced,It is required to balance the data.
# SMOTE function is used to balance the same
train_model1_smote <- SMOTE(renewal ~ ., train_model1, perc.over = 100, perc.under=200)

summary(train_model1_smote$renewal)
# 0    1 
# 6692 6692 

#-----------------------------------------------------#
# Model - 1
initial_model = glm(renewal ~ ., data = train_model1_smote, family = "binomial")
summary(initial_model)

# Model - 2
# We will use Stepwise AIC in this model to remove insignificant independent variables
best_model_1 = step(initial_model, direction = "both")
summary(best_model_1)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(best_model_1)

# Removing premium as it has the largest among all
# P Value is 0.10 which is greater than 0.05 (standard level)

best_model_2 <- glm(formula = renewal ~ perc_premium_paid_by_cash_credit + age_in_days + 
                      Income + Count_3.6_months_late + Count_6.12_months_late + 
                      Count_more_than_12_months_late + application_underwriting_score + 
                      no_of_premiums_paid + sourcing_channel, family = "binomial", 
                    data = train_model1_smote)

summary(best_model_2)
vif(best_model_2)

# Removing sourcing_channel as it has the least predictible capability among all others.

best_model_3 <- glm(formula = renewal ~ perc_premium_paid_by_cash_credit + age_in_days + 
                      Income + Count_3.6_months_late + Count_6.12_months_late + 
                      Count_more_than_12_months_late + application_underwriting_score + 
                      no_of_premiums_paid, family = "binomial", 
                    data = train_model1_smote)

summary(best_model_3)
vif(best_model_3)

# All the variables that are present in best_model_3 seem to be good predictors
# Hence best_model_3 will be our final balanced model.
final_model_balanced <- best_model_3
summary(final_model_balanced)

#-----------------------------------------------------#
#       MODEL EVALUATION LoGISTIC REGRESSION
#-----------------------------------------------------#

predictions_logit_balance <- predict(final_model_balanced,
                                     newdata = test_model1[, -12], type = "response")

summary(predictions_logit_balance)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.004193 0.519151 0.685452 0.628013 0.779898 0.937952

#-----------------------------------------------------#
#           Making the test data ready
#-----------------------------------------------------#

# Checking for duplicate values
dup <- duplicated(test$id)
sum(dup)

# Now lets check the number of NA's in each variable of test set.
sapply(test, function(x) sum(is.na(x)))

#--------------MISSING VALUE TREATMENT----------------------#

# There are NA values in the below variables.
# Count_3.6_months_late - 31 NA's
# Count_6.12_months_late - 31 NA's
# Count_more_than_12_months_late - 31 NA's
# application_underwriting_score - 1323 NA's

# Lets check the missing values in each of the above variables taking Count_3.6_months_late  
Count_3.6_months_late_missing_test <- which(is.na(test$Count_3.6_months_late))
View(test[Count_3.6_months_late_missing_test, ])
length(Count_3.6_months_late_missing_test) # 31

# There are 31 missing values in Count_3.6_months_late, Count_6.12_months_late 
# & Count_more_than_12_months_late which contributes as low as 0.0009 % of total observations
# removing them will not impact the models we create. Hence lets remove them.

test <- test[-which(is.na(test$Count_3.6_months_late)),]

# we are left out with application_underwriting_score as of now on missing values.
# Rechecking for NA values.
sapply(test, function(x) sum(is.na(x)))

# There are 1298 NA values in application_underwriting_score which is 3.7 % of total observations  
# Lets remove these also are they are below the threshold of 5 %

test <- test[-which(is.na(test$application_underwriting_score)),]

#-----------------------------------------------------#

# Creating a copy of test dataset
test_woe <- test

# We will use the traning sets WOE values to populate the variables in test dataset.
empty_matrix_test <- matrix(0, nrow(test_woe), ncol(test_woe))

# This vector will help us in replacing the original values with WOE values
# exclusding id and sourcing_channel, residence_area_type as they are categorical variables.
# we will also take out perc_premium_paid_by_cash_credit, Income & application_underwriting_score
# as the some values need to be adjusted manually.
col_replace_test <- which(!colnames(test_woe) %in% c("id","sourcing_channel","perc_premium_paid_by_cash_credit",
                                                     "Income","residence_area_type","application_underwriting_score"))

# Replacing 8 column data with WOE values.
for(i in col_replace_test){
  test_woe[, i] = woe_function(df=woe_list[[i-1]], variable = test_woe[,i])  
}

# The remaining variables needs to be done manually.
# sourcing_channel
# residence_area_type
# perc_premium_paid_by_cash_credit
# Income
# application_underwriting_score


#-----------------------------------------------------#

# Variable - sourcing_channel

str(test_woe$sourcing_channel)
# Factor w/ 5 levels "A","B","C","D",..: 3 1 3 1 2 2 2 1 1 1 ...

# Below is the woe values for sourcing channel
#sourcing_channel         WOE
#1                A  0.15189824
#2                B -0.03663604
#3                C -0.19501574
#4                D -0.32265505
#5                E -0.21345988

# Storing the levels
levels(test_woe$sourcing_channel) <-c(0.15189824,-0.03663604,-0.19501574,-0.32265505,-0.21345988)

# Changing to WOE values
test_woe$sourcing_channel <- as.numeric(levels(test_woe$sourcing_channel))[test_woe$sourcing_channel]

#-----------------------------------------------------#

# Variable - residence_area_type

str(test_woe$residence_area_type)
# Factor w/ 2 levels "Rural","Urban": 2 2 1 2 2 1 2 2 2 1 ...

# Below is the woe values for residence_area_type
#residence_area_type         WOE
#1               Rural -0.01139561
#2               Urban  0.00755598

# Storing the levels
levels(test_woe$residence_area_type) <-c(-0.01139561,0.00755598)

# Changing to WOE values
test_woe$residence_area_type <- as.numeric(levels(test_woe$residence_area_type))[test_woe$residence_area_type]

#-----------------------------------------------------#

# Variable - application_underwriting_score

str(test_woe$application_underwriting_score)

# Below is the woe values for application_underwriting_score
#application_underwriting_score         WOE
#1                              NA  0.13611435
#2                    [91.9,98.28] -0.63557309
#3                   [98.29,98.68] -0.34552064
#4                    [98.69,98.9] -0.07030956
#5                   [98.91,99.06]  0.05491282
#6                    [99.07,99.2]  0.22795486
#7                   [99.21,99.33]  0.32268251
#8                   [99.34,99.46]  0.23130161
#9                    [99.47,99.6]  0.13166308
#10                  [99.61,99.78]  0.09351728
#11                  [99.79,99.89]  0.32724032

test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 91.9 & 
                                                test_woe$application_underwriting_score <=98.28)] <- '-0.63557309'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 98.29 & 
                                                test_woe$application_underwriting_score <=98.68)] <- '-0.34552064'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 98.69 & 
                                                test_woe$application_underwriting_score <=98.9)] <- '-0.07030956'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 98.91 & 
                                                test_woe$application_underwriting_score <=99.06)] <- '0.05491282'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.07 & 
                                                test_woe$application_underwriting_score <=99.2)] <- '0.22795486'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.21 & 
                                                test_woe$application_underwriting_score <=99.33)] <- '0.32268251'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.34 & 
                                                test_woe$application_underwriting_score <=99.46)] <- '0.23130161'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.47 & 
                                                test_woe$application_underwriting_score <=99.6)] <- '0.13166308'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.61 & 
                                                test_woe$application_underwriting_score <=99.78)] <- '0.09351728'
test_woe$application_underwriting_score[which(test_woe$application_underwriting_score >= 99.79 & 
                                                test_woe$application_underwriting_score <=99.89)] <- '0.32724032'

#-----------------------------------------------------#

# Variable - Income

str(test_woe$Income)

woe_list[[3]]
#Income         WOE
#1      [24030,71540] -0.36438592
#2      [71550,96120] -0.35303253
#3     [96130,120070] -0.18546369
#4    [120080,143120] -0.15488939
#5    [143130,166710] -0.01711131
#6    [166740,195120]  0.03780812
#7    [195130,231330]  0.24562210
#8    [231360,279040]  0.35925354
#9    [279050,357540]  0.40970928
#10 [357550,90262600]  0.36967359

# Lets scale Income first
test_woe$Income <- test_woe$Income/100000

# creating buckets
test_woe$Income <- cut(test_woe$Income,
                       c(.2402,.7154,.9612,1.2007,1.4312,1.6671,1.9512,2.3133,2.7904,3.5754,902.7),
                       include.lowest = FALSE)

# Now we will rename the levels in the newly created bins
test_woe$Income <- as.character(test_woe$Income)
test_woe$Income[which(test_woe$Income == "(0.24,0.715]")] <- "-0.36438592"
test_woe$Income[which(test_woe$Income == "(0.715,0.961]")] <- "-0.35303253"
test_woe$Income[which(test_woe$Income == "(0.961,1.2]")] <- "-0.18546369"
test_woe$Income[which(test_woe$Income == "(1.2,1.43]")] <- "-0.15488939"
test_woe$Income[which(test_woe$Income == "(1.43,1.67]")] <- "-0.01711131"
test_woe$Income[which(test_woe$Income == "(1.67,1.95]")] <- "0.03780812"
test_woe$Income[which(test_woe$Income == "(1.95,2.31]")] <- "0.24562210"
test_woe$Income[which(test_woe$Income == "(2.31,2.79]")] <- "0.35925354"
test_woe$Income[which(test_woe$Income == "(2.79,3.58]")] <- "0.40970928"
test_woe$Income[which(test_woe$Income == "(3.58,903]")] <- "0.36967359"

#-----------------------------------------------------#

# Variable - perc_premium_paid_by_cash_credit

str(test_woe$perc_premium_paid_by_cash_credit)

woe_list[[1]]
#perc_premium_paid_by_cash_credit        WOE
#1                             [0,0]  0.8307599
#2                          [0,0.02]  1.5956447
#3                       [0.02,0.05]  1.3530322
#4                       [0.05,0.09]  1.0545986
#5                       [0.09,0.17]  0.8280239
#6                       [0.17,0.28]  0.5511703
#7                       [0.28,0.44]  0.1338274
#8                       [0.44,0.66] -0.3441148
#9                       [0.66,0.92] -0.9080382
#10                         [0.92,1] -1.2926078

# creating buckets
test_woe$perc_premium_paid_by_cash_credit <- cut(test_woe$perc_premium_paid_by_cash_credit,
                                                 c(-.99,.003,.021,.048,.092,.166,.279,.437,.655,.921,1.10),
                                                 include.lowest = FALSE)

# Now we will rename the levels in the newly created bins
test_woe$perc_premium_paid_by_cash_credit <- as.character(test_woe$perc_premium_paid_by_cash_credit)
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(-0.99,0.003]")] <- "0.8307599"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.003,0.021]")] <- "1.5956447"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.021,0.048]")] <- "1.3530322"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.048,0.092]")] <- "1.0545986"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.092,0.166]")] <- "0.8280239"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.166,0.279]")] <- "0.5511703"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.279,0.437]")] <- "0.1338274"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.437,0.655]")] <- "-0.3441148"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.655,0.921]")] <- "-0.9080382"
test_woe$perc_premium_paid_by_cash_credit[which(test_woe$perc_premium_paid_by_cash_credit == "(0.921,1.1]")] <- "-1.2926078"

#-----------------------------------------------------#

test_woe$perc_premium_paid_by_cash_credit <- as.numeric(test_woe$perc_premium_paid_by_cash_credit)
test_woe$Income <- as.numeric(test_woe$Income)
test_woe$application_underwriting_score <- as.numeric(test_woe$application_underwriting_score)


# Storing only the id column of test_woe
test_woe_id <- as.data.frame(test_woe[,1])

# Predicting on the test_woe dataset
predictions_logit_balance1 <- predict(final_model_balanced,
                                     newdata = test_woe[,-1], type = "response")

summary(predictions_logit_balance1)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.004154 0.529977 0.668741 0.620224 0.763655 0.942117

# Below is the validation set summary

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.004193 0.519151 0.685452 0.628013 0.779898 0.937952

# Looks to be a close match ...

pred_values <- as.data.frame(predictions_logit_balance1)

#-----------------------------------------------------#

subimission_raw <- cbind(test[,1], pred_values)

#-----------------------------------------------------#

write.csv(subimission_raw, "Submission.csv", row.names = FALSE)
