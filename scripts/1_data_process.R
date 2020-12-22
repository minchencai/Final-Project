#### Preamble ####
# Purpose: Prepare Survey data, using election opinion polls, cesR
# Author: Minchen Cai 
# Data: December 9th 2020
# Contact: Minchen Cai, university of toronto, https://github.com/minchencai/Final-Project [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded library cesR
# - Don't forget to gitignore it!
# the census data 2016, age 15 in 2016 is age 18 in 2019 
# Open access code 

getwd()
#### Workspace setup ####
require(cesR)
library(forcats)
require(labelled)
library(tidyverse)
require(magrittr)
# load ces2019 web data

# get the not in operator
`%nin%` = Negate(`%in%`)

get_ces("ces2019_web")
# Read in the raw data (You might need to change this if you use a different dataset)
# Add the labels, convert to a factor type   
raw_data_survey <- labelled::to_factor(ces2019_web)
# Just keep some variables
# check variables 
colnames(raw_data_survey)
# based on the most loved forecasting paper
# votechoice = (age employment province) gender education
# state, education, sex/gender, age, race, household_income, employment
reduced_data_survey <- 
  raw_data_survey%>% 
  select(
         # interest,
         # registration,
         # vote_2016,
         cps19_votechoice,
         cps19_citizenship,
         cps19_yob, 
         cps19_gender, 
         cps19_province,
         cps19_education,
         cps19_employment
    # ,cps19_income_num
       ) %>%  mutate(cps19_yob = as.numeric(cps19_yob)) %>% mutate(age = 100 - cps19_yob)  %>%  
    select(-cps19_yob) %>% 
  # rename to match 
  rename(vote = cps19_votechoice, citizen = cps19_citizenship, Gender = cps19_gender,
         Province = cps19_province, Education = cps19_education, Employment = cps19_employment) %>%
  filter(age > 17) %>% drop_na()

# check dataset
# head(reduced_data_survey)

  # filter based on eligibility and age 
       # filter(age > 17) %>% filter(vote_intention != "No, I am not eligible to vote") %>% 
       # drop_na()

# citizen
reduced_data_survey$citizen %>% levels() 
# gender 
reduced_data_survey$Gender %>% levels()
# education 
reduced_data_survey$Education %>% levels()
# levels 
reduced_data_survey$Employment %>% levels()
# EDA of relevant variables 
reduced_data_survey$vote %>% levels()

# only eligible respondents, then remove this feature 
reduced_data_survey <-  reduced_data_survey %>% filter(citizen != "Other") %>% select( -citizen ) %>% 
  # filter, 
  # Is vote a binary? If not, what are you going to do?
  # make vote a binary 
  filter(vote %in% c('Liberal Party', 'Conservative Party') ) %>% 
  # make binary party, only liberal and conservative, categorized 
  mutate(Vote = relevel(vote, ref = "Conservative Party"))  %>%
  # make binary gender 
     select(-vote) %>% filter(Gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)") %>%
       mutate(Gender = ifelse(Gender == "A man", "Man", "Woman")) %>% 
  # make binary age 
        mutate(Age = ifelse(age < 65, "15-64", "65-" ) ) %>% 
  # employment categorized to employed unemployed
        filter(Employment %nin% c("Other (please specify)", "Don't know/ Prefer not to answer" ) ) %>% 
        mutate(Employment = 
                 ifelse(Employment == "Unemployed/ looking for work", "Unemployed", "Employed") ) %>% 
   # education 
        filter(Education != "Don't know/ Prefer not to answer" ) %>% 
        mutate(Education = forcats::fct_collapse(Education,
                              "Under High School" =  c("No schooling", 
                                                       "Some elementary school" ,
                                                       "Completed elementary school",
                                                       "Some secondary/ high school" ),
                              "High School" = c("Completed secondary/ high school",
                                "Some technical, community college, CEGEP, College Classique"),
                              "Post High School" = c("Completed technical, community college, CEGEP, College Classique",
                                                     "Bachelor's degree",
                                                     "Master's degree",
                                                     "Professional degree or doctorate",
                                                     "Some university")  ) ) 
   # income 
reduced_data_survey %>% head()
# votechoice = (age employment province) gender education

survey_data <- reduced_data_survey %>% select(Vote, Age, Gender, Province, Education, Employment)

# check levels 
survey_data$Vote %<>% factor()
survey_data$Age %<>% factor()
survey_data$Gender %<>% factor()
survey_data$Province %<>% factor()
survey_data$Education %<>% factor()
survey_data$Employment %<>% factor()

# apply factor(), apply levels()


# check levels in factor
survey_data$Vote %>% levels()
survey_data$Age %>% levels()
survey_data$Gender %>% levels()
survey_data$Province %>% levels()
survey_data$Education %>% levels()
survey_data$Employment %>% levels()

# barplot to briefly check the distribution of the categorical variables 
barplot(table(survey_data$Vote   ) )
barplot(table(survey_data$Age ) )
barplot(table(survey_data$Gender ) )
barplot(table(survey_data$Province ) )
barplot(table(survey_data$Education ) )
barplot(table(survey_data$Employment ) )

# save datasets 
write_csv(survey_data, "inputs/data/survey.csv")

write_csv(survey_data, "outputs/survey.csv")





