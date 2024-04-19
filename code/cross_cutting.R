library(tidyverse)
library(expss)
library(labelled)


#CC-1.1: Beneficiaries reporting safety concerns 

#Load Data set
data <- read_csv("~/GitHub/RAMResourcesScripts/Static/PROP_AAP_CRF_Sample_Survey.csv")

#assign variable and value labels
var_label(data$HHAsstSecurity) <- "Have you or any of your household members experienced any security challenge related to WFP assistance?"
val_lab(data$HHAsstSecurity) = num_lab("
             0 No
             1 Yes
             888 Don't know
")


#creates a table of the weighted percentage of HHAsstSecurity by
#creating a temporary variable to display value labels 
#and providing the option to use weights if needed


HHAsstSecurity_table_wide <- data %>% 
  drop_na(HHAsstSecurity) %>%
  count(HHAsstSecurity_lab = as.character(HHAsstSecurity)) %>% # if weights are needed use instead the row below 
  #count(HHAsstSecurity_lab = as.character(HHAsstSecurity), wt = nameofweightvariable)
  mutate(Percentage = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = HHAsstSecurity_lab,
              values_from = Percentage,
              values_fill =  0) 


#CC 1.2

#Load survey data

data <- read_csv("Static/PROP_AAP_CRF_Sample_Survey.csv")

#assign variable and value labels
var_label(data$HHAsstAccess) <- "Have you or any member of your household been unable to access WFP assistance one or more times?"
val_lab(data$HHAsstAccess) = num_lab("
             0 No
             1 Yes
             888 Don't know
")


#creates a table of the weighted percentage of HHAsstAccess by
#creating a temporary variable to display value labels 
#and providing the option to use weights if needed


HHAsstAccess_table_wide <- data %>% 
  drop_na(HHAsstAccess) %>%
  count(HHAsstAccess_lab = as.character(HHAsstAccess)) %>% # if weights are needed use instead the row below 
  #count(HHAsstAccess_lab = as.character(HHAsstAccess), wt = nameofweightvariable)
  mutate(Percentage = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = HHAsstAccess_lab,
              values_from = Percentage,
              values_fill =  0) 


#CC 1.3
#add sample data
data <- read_csv("Static/PROP_AAP_CRF_Sample_Survey.csv")

#assign variable and value labels
var_label(data$HHAsstRespect) <- "Do you think WFPandor partner staff have treated you and members of your household respectfully?"
var_label(data$HHDTPDign) <- "Do you think the conditions of WFP programme sites are dignified?"

data <- data %>%
  mutate(across(c(HHAsstRespect, HHDTPDign), ~labelled(., labels = c(
    "No" = 0,
    "Yes" = 1
  ))))

#calculate indicator and assign variable label & name
data <- data %>% mutate(HHAsstRespectDign = case_when(
  HHAsstRespect == 1 & HHDTPDign == 1 ~ 1,
  TRUE ~ 0
))
var_label(data$HHAsstRespectDign) <- "Treated with respect while engaging in WFP programs"
val_lab(data$HHAsstRespectDign) = num_lab("
             0 No
             1 Yes
")


#creates a table of the weighted percentage of HHAsstRespectDign by
#creating a temporary variable to display value labels 
#and providing the option to use weights if needed


HHAsstRespectDign_table_wide <- data %>% 
  drop_na(HHAsstRespectDign) %>%
  count(HHAsstRespectDign_lab = as.character(HHAsstRespectDign)) %>% # if weights are needed use instead the row below 
  #count(HHAsstRespectDign_lab = as.character(HHAsstRespectDign), wt = nameofweightvariable)
  mutate(Percentage = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = HHAsstRespectDign_lab,
              values_from = Percentage,
              values_fill =  0) 