library(tidyverse)
library(RM.weights)
library(skimr)
   

#Load data and select necessary variables
FullFIESData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% #Add the data file path here
  #Select necessary variables
  select(ADMIN4Name, ACName, HHID, HHList, HHBaseline,HHHEthnicity, HHHLanguage,
         IDPoor, SEX_HHH, starts_with("FIES")) %>% 
  # Remove underscores from the variable names
  rename_with(~str_remove_all(., "_"), starts_with("FIES")) %>% 
  # Change values that are not 1 or 0 to NA in the FIES variables
  mutate(across(starts_with("FIES"), ~case_when(. %in% c(1, 0) ~ ., TRUE ~ NA)))

##Analyse data
FIESData <- FullFIESData %>% 
  #Remove outliers
  # find_outliers() %>%
  # Select only the FIES variables
  select(starts_with("FIES"))

# FIES Model

FIESModel = RM.w(FIESData[,-1])


round(cbind("severity" = FIESModel$b, "infit" = FIESModel$infit), 2)

screeplot(prcomp(FIESModel$mat.res), type = "lines")

FIESModel$reliab.fl





