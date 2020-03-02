library(dplyr)

adult <- adult %>%
  rename(age = V1,
         workclass = V2,
         fnlwgt = V3,
         education = V4,
         'education-num' = V5,
         'marital-status' = V6,
         occupation = V7,
         relationship = V8,
         race = V9,
         sex = V10,
         'capital-gain' = V11,
         'capital-loss' = V12,
         'hours-per-week' = V13,
         'native-country' = V14,
         salary = V15)
#------------ cleaning --------------
library(dplyr)
adult <- adult %>% 
  filter(
    !is.na(age),
    !is.na(workclass),
    !is.na(fnlwgt),
    !is.na(education),
    !is.na("education-num"),
    !is.na("marital-status"),
    !is.na(occupation),
    !is.na(race),
    !is.na(sex),
    !is.na(relationship),
    !is.na("capital-gain"),
    !is.na("capital-loss"),
    !is.na("hours-per-week"),
    !is.na("native-country"),
    !is.na(salary)
  )

# find and remove rows with character ?
library(dplyr)
adult <- adult %>% 
  filter(
    age != ' ?',
    workclass != ' ?',
    fnlwgt != ' ?',
    education != ' ?',
    "education-num" != ' ?',
    "marital-status" != ' ?',
    occupation != ' ?',
    race != ' ?',
    sex != ' ?',
    relationship != ' ?',
    "capital-gain" != ' ?',
    "capital-loss" != ' ?',
    "hours-per-week" != ' ?',
    "native-country" != ' ?',
    salary != ' ?'
  )

# find and count duplicated rows
library(dplyr)
library(tidyr)
duplicates <- adult %>% 
  duplicated() %>%
  table()
duplicates
adult <- adult %>% 
  distinct()

#remove row with missing values
library(dplyr)
library(tidyr)
adult <- adult %>% drop_na()

#-------------- histogramas -----------------------

library(devtools)
install_github("kassambara/easyGgplot2")

library(easyGgplot2)

ggplot2.histogram(data=adult, xName='age',
                 groupName='salary', legendPosition="top",
                 alpha=0.5 )

ggplot2.histogram(data=adult, xName='age',
                  groupName='sex', legendPosition="top",
                  alpha=0.5 )

