## SPRINGBOARD UNIT 7 -- APPLY ML TO CAPSTONE
# Tonka Kozarova Fenichel
# 7/8/2018

##########################################################################################################
##    working notes -- do not run

#Apply machine learning
#Now that you've learned the basics of machine learning, it's time to actually apply it to your capstone project!
#1 Project: Apply machine learning to your capstone project
#1 - 2 Hours

#You've collected your data, cleaned it up, wrangled it into shape and explored it. Now it's time to perform some in-depth data #analysis #using machine learning. This step depends on you and your mentor, but here are some suggestions to get you going.

#How do you frame your main question as a machine learning problem? Is it a supervised or unsupervised problem? If it is supervised, #is #it a regression or a classification?
#What are the main features (also called independent variables or predictors) that you'll use?
#Which machine learning technique will you use?
#How will you evaluate the success of your machine learning technique? What metric will you use?
#Submission


#We'd like you to think about your machine learning approach to your project at a high level. 
#Summarize your answers to the questions above in a short (1-2 page max) document. You may use any format of your choice (R Markdown, #Google Docs, Word etc).
#Add this document to the github repository for your project.
#Submit a link to this document.
#This document will be incorporated into your final project report.


#########################################################################################################

## My Capstone Project is focused on data exploration and visualization in RSiny, ratere than machine learning and predictive 
## statistics.  This is mainy due to te nature of the data.  
## My original project objectives including a machine learning component,
## but when working with the dataset I learned that a meaningful statistical model prescriptive anlaytics would be best wih
## bringing in additional dataset like census data, highway network, or electric vehicles sales data, wich would end up 
## being a project outside of the core scope of tis course and more time intense than we planned for.

## Below is my attempt to play wit the dataset and witin the scope of ML material we covered in this course:

#########################################################################################################



library(jsonlite)
library(dplyr)
library(readr)

ecs_api_orig_df <- stream_in(url("https://api.openchargemap.io/v2/poi/?output=json&maxresults=80000&compact=true&verbose=false"))

ecs_api_df <- cbind(
  ecs_api_orig_df$OperatorID,
  ecs_api_orig_df$UsageTypeID,
  ecs_api_orig_df$AddressInfo[,1],
  ecs_api_orig_df$AddressInfo[,2],
  ecs_api_orig_df$AddressInfo[,3],
  ecs_api_orig_df$AddressInfo[,4],
  ecs_api_orig_df$AddressInfo[,5],
  ecs_api_orig_df$AddressInfo[,6],
  ecs_api_orig_df$AddressInfo[,7],
  ecs_api_orig_df$AddressInfo[,8],
  ecs_api_orig_df$AddressInfo[,9],
  ecs_api_orig_df$NumberOfPoints,
  ecs_api_orig_df$StatusTypeID,
  ecs_api_orig_df$DateLastStatusUpdate,
  ecs_api_orig_df$DataQualityLevel,
  ecs_api_orig_df$DateCreated,
  ecs_api_orig_df$SubmissionStatusTypeID,
  ecs_api_orig_df$IsRecentlyVerified,
  ecs_api_orig_df$DateLastVerified,
  ecs_api_orig_df$DataProvidersReference,
  ecs_api_orig_df$DateLastConfirmed
)


colnames(ecs_api_df) <- c("operatorID",
                          "UsageTypeID",
                          "AddressInfoID",
                          "AddressInfoTitle",
                          "AddressInfoAddressLine1",
                          "AddressInfoTown",
                          "AddressInfoStateOrProvince",
                          "AddressInfoPostcode",
                          "AddressInfoCountryID",
                          "AddressInfoLatitude",
                          "AddressInfoLongitude",
                          "NumberOfPoints",
                          "StatusTypeID",
                          "DateLastStatusUpdate",
                          "DataQualityLevel",
                          "DateCreated",
                          "SubmissionStatusTypeID",
                          "IsRecentlyVerified",
                          "DateLastVerified",
                          "DataProvidersReference",
                          "DateLastConfirmed"
)

ecs_api_df <- as_data_frame(ecs_api_df)

write_csv(ecs_api_df, "ecs_api_df.csv" )

df <- read_csv("ecs_api_df.csv")

#######################################################

summary(df)

## What is the correlation between the country and operator ID?
## A: Weak (0.072) 
cor(df$AddressInfoCountryID, df$operatorID, use = "complete.obs")


## Insights from regression between the country and operators' points ranking:
## negative slope, but significant relationship. 

## Insights from regression between the country and operators' points ranking:
## negative slope, but significant relationship. 
lm1 <- lm(AddressInfoCountryID ~ NumberOfPoints, # regression formula
          data=df) # data set
# Summarize and print the results
summary(lm1) # show regression coefficients table
plot(lm1)

## Insights from regression between the country and operators' points ranking and usage type ID:
## now negative slope for NumberofPoints, but positive for UsageTypeID, but error margins are small and p-values indicate 
## significant varialbles. 
lm2 <- lm(AddressInfoCountryID ~ NumberOfPoints + UsageTypeID, # regression formula
          data=df) # data set
## Summarize and print the results
summary(lm2) # show regression coefficients table
plot(lm2)

## Adding the interaction:
lm1i <- lm(AddressInfoCountryID ~ NumberOfPoints*UsageTypeID, 
           data=df) 

## View results:
## Note: interaction did not increase the model accuracy.
coef(summary(lm1i)) # show regression coefficients table


####  LOG REG ####

## Review of var levels.
## Note: Really no variables with levels I can use for glm:

levels(df$operatorID)
levels(df$UsageTypeID)
levels(df$AddressInfoID)
levels(df$AddressInfoTitle)
levels(df$AddressInfoCountryID)
levels(df$NumberOfPoints)
levels(df$StatusTypeID)
levels(df$DataQualityLevel)
levels(df$IsRecentlyVerified)
levels(df$SubmissionStatusTypeID)


## I tried to recode 'IsRecentlyVerified' from TRUE/FALSE to 0/1, but got an error about logical data not being able to recode like that.
## I don't see a practical application of a glm here.