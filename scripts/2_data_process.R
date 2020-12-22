#### Preamble ####
# Purpose: Prepare, simulate CENSUS 2016 Canada, based on Statistics Canada [...UPDATE ME!!!!!]
# Author: Minchen Cai [CHANGE THIS TO YOUR NAME!!!!]
# Data: December 9th 2020
# Contact: Minchen Cai, university of toronto [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded/retrieved the census profile from canadian census analyser
# - Don't forget to gitignore it!
# - Open access code 

getwd()
#### Workspace setup ####
library(tidyverse)
library(forcats)
require(magrittr)
# Read in the raw data. the data are not at individual level, 
# 2016 Canadian Census data
# using the province or country level summary census data to simulate data 
census_profile <- read_csv("inputs/data/byprovince.csv") %>% select(COL2, COL3, COL4,
                                                                    COL5, COL21, COL22, COL23) 


# votechoice = (age unemployment province) gender education
# first simulate age, gender, province for all ages then 
# employment, education and income is for age 15 and over 
colnames(census_profile) <- c("Province", "Population", "15-64", "65-", 
                              "High_School_Diploma", 
                              "No_High_School_Diploma", "Post_High_School_Diploma")

# header file (column header)
census_profile_header <- read_table("inputs/data/byprovince.txt")
census_profile %<>% arrange(Province)
  
# Total number of simulated data
N <- 1000000
# match the province names of two datasets 
census_profile[14, 1] <- "Yukon"
# get provinces 
# # votechoice = (age employment province) gender education
provinces <- census_profile[census_profile$Province != "CANADA",] %>% dplyr::pull(Province)

# total population 
total_popu <- census_profile[census_profile$Province == "CANADA", "Population"] %>% as.numeric()

# sum of province == total population 
sum(census_profile[census_profile$Province != "CANADA", "Population"]) == total_popu

# get all proportions for simulation, simulation using sample, we need prob 
# age 15 - 64 in 2016 is age 18-67 in 2019 
# votechoice = (age employment province) sex education
# https://www.statista.com/statistics/442316/canada-unemployment-rate-by-provinces/

province_population <- census_profile[census_profile$Province != "CANADA", "Population"] %>% as.vector()

prop_province <- tibble( 
                      Province = provinces %>% as.vector(),
                         province_population /total_popu %>% as.vector(),
                         Unemployment = c(0.069, 0.047, 0.053, 0.079, 0.119, 0.082,
                                          0.072, 0.134, 0.056, 0.088, 0.051, 0.054, 0.036),
                         census_profile[census_profile$Province != "CANADA", "15-64"]/province_population,
                         census_profile[census_profile$Province != "CANADA", "65-"]/province_population )
prop_province
# from statcan
# canada country level proportions for simulation  
# votechoice = (age employment province) sex education
# statcan.gc.ca
man <- 17264200
woman <- 17887530 
all_education <- sum( census_profile[census_profile$Province == "CANADA", 
                                     c("High_School_Diploma", "No_High_School_Diploma", "Post_High_School_Diploma") ] )
prop_overall <- tibble(gender = c(man/(man + woman), woman/(man + woman), 0) ,
                       education = c(as.numeric( census_profile[census_profile$Province == "CANADA", "No_High_School_Diploma"]/all_education) , 
                       0.265 + 0.098, 
                       1 - 0.363 - as.numeric( census_profile[census_profile$Province == "CANADA", "No_High_School_Diploma"]/all_education) ) )

prop_overall

# Just keep some variables that may be of interest 
# random seed to make reproducible 
set.seed(666)
reduced_data_census <- tibble(id = 1:N,
                              Province = sample(prop_province$Province, N, replace = TRUE,
                                                prob = prop_province$Population), 
                              Gender = sample(c("Man", "Woman"), N, replace = TRUE, 
                                              prob = prop_overall$gender[1:2]) 
                              ) %>% arrange(Province)
head(reduced_data_census, 10)
# sample at province level 
# make a list then combine elements of list to data frame
datalist = list()
for(i in 1:length(provinces) ){
  province = provinces[i] 
  n = nrow(reduced_data_census[reduced_data_census$Province == province, ])
  propAge65 = prop_province[prop_province$Province == province, "65-"] %>% as.numeric()
  propAge64 = prop_province[prop_province$Province == province, "15-64"] %>% as.numeric()
  propAge14 = 1 - propAge64 - propAge65
 
  datalist[[i]] = data.frame(Age = sample(c("-14", "15-64", "65-"), n, replace = TRUE, 
                              prob =  c(propAge14, propAge64, propAge65) )   )
  
}
# combine dataset of each province 
df = do.call(rbind, datalist)
# remove age under 14 
dfage14 = cbind(reduced_data_census, df) %>% filter(Age != "-14")
# new total population 
Ntmp = nrow(dfage14)
# simulation of education level 
dfage14$Education = sample(c("Under High School", "High School", "Post High School"), 
                           Ntmp, replace = TRUE, prob = dplyr::pull(prop_overall, education ))
dfage14 %>% head(5)
# simulate by province unemployment rate, for age 15 and above  
dtlist <- list()
for(i in 1:length(provinces) ){
  province = provinces[i] 
  n = nrow(dfage14[dfage14$Province == province, ])
  # probability of that province, unemployment rate
  propUnemploy = prop_province[prop_province$Province == province, "Unemployment"] %>% as.numeric()
  dtlist[[i]] = data.frame( Employment = sample( c("Employed", "Unemployed"), n, replace = TRUE,  
                                                 prob = c(1- propUnemploy, propUnemploy) )  )
  
}
dfUnemploy = do.call(rbind, dtlist)

census_data = cbind(dfage14, dfUnemploy) %>% drop_na()
census_data %>% head(10)
# make sure all categorical variable

census_data$Province %<>% as.factor()
census_data$Gender %<>% as.factor()
census_data$Age %<>% as.factor()
census_data$Employment %<>% as.factor()
census_data$Education %<>% as.factor()

# check levels 
barplot(table(census_data$Province) )
barplot(table(census_data$Gender ) )
barplot(table(census_data$Age) )
barplot(table(census_data$Employment ) )
barplot(table(census_data$Education) )

# check levels 
census_data$Employment %>% levels()
census_data$Education %>% levels()

# relevel, make references group 
# relevel the household_income and education
census_data$Employment <- factor(census_data$Employment,
                                               levels(census_data$Employment)[c(2, 1)])
census_data$Education <- factor(census_data$Education,
                                        levels(census_data$Education)[c(3, 1:2)])


# recheck levels 
census_data$Employment %>% levels()
census_data$Education %>% levels()

census_data <- census_data %>% select(id, Age, Gender, Province, Education, Employment)

# save datasets
write_csv(census_data, "inputs/data/census.csv")
write_csv(census_data, "outputs/census.csv")




