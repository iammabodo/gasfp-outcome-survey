# Load necessary packages

library(tidyverse)
library(readxl)
library(janitor)
library(gt)

    
# Import the data

EconomicEmpowermentData <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>% 
  # Select relevant columns to calculate the percentage of men and women reporting to have economic empowerment
  select(HHID, HHHSex, HHHEthnicity, HHHLanguage, IDPoor, # Dis aggregation variables
         RFinancSitGender, HHGenMembers, RFinancSit, RFinancSitRsn, LadderToday, Ladder1YearAgo, LadderReason) %>% # Indicator calculation variables
  # Mutate the variable to have meaningful labels
  mutate(HHHSex = case_when(
    HHHSex == 0 ~ "Female",
    HHHSex == 1 ~ "Male"),
    HHHEthnicity = case_when(
    HHHEthnicity == 1 ~ "Khmer",
    TRUE ~ "Non Khmer"),
    HHHLanguage = case_when(
    HHHLanguage == 1 ~ "Khmer",
    HHHLanguage == 2 ~ "Bunong",
    TRUE ~ "Other"),
    IDPoor = case_when(
    IDPoor == 1 ~ "Yes",
    TRUE ~ "No"),
    RFinancSitGender = case_when(
    RFinancSitGender == 0 ~ "Female",
    RFinancSitGender == 1 ~ "Male"),
    RFinancSit = case_when(
    RFinancSit == 1 ~ "Improved",
    RFinancSit == 2 ~ "Stayed the same",
    RFinancSit == 3 ~ "Worsened",
    TRUE ~ "Prefer not to answer")) %>%
  # Create the economic empowerment variable
  mutate(EconomicEmpowerment = case_when(
    RFinancSit == "Improved" & Ladder1YearAgo <= LadderToday ~ "Economically Empowered",
    TRUE ~ "Not Economically Empowered"))


# Calculate the percentage of people reporting that the financial situation has improved, stayed the same, or worsened
EconomicEmpowermentData %>% 
  group_by(IDPoor, EconomicEmpowerment) %>% # To include the disaggregation by gender of the respondent here once we have the final data
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)






