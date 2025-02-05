##################################################################
##                     Demographic overview                     ##
##################################################################

## load packages
source("scripts/utils/check_pkgs.R")

## leoad demographics data
data_demographics <- read_csv(file = paste0(here(), "/data/data_social_all_participants_full_sample_demographics"))
data_after_cleaning <- read_csv(file = paste0(here(), "/data/social/data_social_all_participants_08-2024.csv"))

## demographics before excluding participants

## adults gender and age
data_demographics %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_demographics %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))


## adolescents gender and age
data_demographics %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_demographics %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))


## demographics after excluding participants

## adults gender and age
data_after_cleaning %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_after_cleaning %>% 
  select(group, gender, uniqueID, age) %>% 
  filter(group=="adults") %>% 
  distinct() %>% 
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))

## adolescents gender and age
data_after_cleaning %>% 
  select(group, gender, uniqueID) %>% 
  filter(group=="adolescents") %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(count = n())

data_after_cleaning %>% 
  select(group, uniqueID, age) %>% 
  dplyr::filter(group=="adolescents") %>% 
  distinct() %>%
  reframe( mean = mean(age, na.rm=TRUE),
           sd = sd(age, na.rm =TRUE),
           range = range(age, na.rm = TRUE))