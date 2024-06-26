# Load necessary packages

library(tidyverse)
library(readxl)
library(janitor)
library(gt)

    
# Import the data

EconomicEmpowermentData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Select relevant columns to calculate the percentage of men and women reporting to have economic empowerment
  select(interview_key, HHID, Sex, HHHEthnicity, HHHLanguage, IDPoor, HHBaseline, # Dis aggregation variables
        HHGenMembers, RFinancSit, RFinancSitRsn, LadderToday, Ladder1YearAgo, LadderReason) %>% # Indicator calculation variables
  # Mutate the variable to have meaningful labels
  mutate(Sex = case_when(
    Sex == 0 ~ "Female",
    Sex == 1 ~ "Male"),
    HHHEthnicity = case_when(
      HHHEthnicity == 1 ~ "Ethnic Majority",
      HHHEthnicity == 2  | HHHEthnicity == 4  | HHHEthnicity == 5 | 
      HHHEthnicity == 6 | HHHEthnicity == 7 | HHHEthnicity == 8 | 
      HHHEthnicity == 9 | HHHEthnicity == 10 ~ "Ethnic Minority",
      TRUE ~ NA),
    HHBaseline = case_when(
    HHBaseline == 0 ~ "Baseline Members",
    HHBaseline == 1 ~ "New Members",
    TRUE ~ "Don't Know"),
    HHHLanguage = case_when(
    HHHLanguage == 1 ~ "Khmer",
    HHHLanguage == 2 ~ "Bunong",
    TRUE ~ "Other"),
    IDPoor = case_when(
    IDPoor == 1 ~ "Yes",
    TRUE ~ "No"),
    RFinancSit = case_when(
    RFinancSit == 1 ~ "Improved",
    RFinancSit == 2 ~ "Stayed the same",
    RFinancSit == 3 ~ "Worsened",
    TRUE ~ "Prefer not to answer")) %>%
  # Create the economic empowerment variable. This is the key indicator for the analysis
  mutate(EconomicEmpowerment = case_when(
    RFinancSit == "Improved" & Ladder1YearAgo <= LadderToday ~ "Economically Empowered",
    TRUE ~ "Not Economically Empowered")) %>% 
  # Change all character variables to factors
  mutate_if(is.character, as.factor)


# Calculate the percentage of people reporting that the financial situation has improved, stayed the same, or worsened
GenderEconomicEmpGap <- EconomicEmpowermentData %>% 
  group_by(Sex, EconomicEmpowerment) %>% # To include the disaggregation by gender of the respondent here once we have the final data
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  # round the percentage to 2 decimal places
  mutate(Percentage = round(Percentage, 2))%>% 
  rename(Disagregation = Sex)

GenderEconomicEmpGapBaseline <- EconomicEmpowermentData %>% 
  group_by(HHBaseline, EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>% 
  filter(HHBaseline != "Don't Know")%>% 
  rename(Disagregation = HHBaseline)

GenderEconomicEmpGapEthnicity <- EconomicEmpowermentData %>%
  group_by(HHHEthnicity, EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  rename(Disagregation = HHHEthnicity) %>% 
  filter(Disagregation != "NA")

GenderEconomicEmpGapTot <- EconomicEmpowermentData %>%
  group_by(EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, EconomicEmpowerment, Count, Percentage)

# Combine the three tables into one

GenderEconomicEmpGapFull <- bind_rows(GenderEconomicEmpGap, GenderEconomicEmpGapBaseline, GenderEconomicEmpGapEthnicity, GenderEconomicEmpGapTot) 


# Write this into an xlsx file
write.xlsx(GenderEconomicEmpGapFull, "report/GenderEconomicEmpGapFull.xlsx")

#####################################################END OF DATA CLEANING######################################################################

# Visualize the table using ggplot 
GenderEconomicEmpGap %>%
  ggplot(aes(Sex, Percentage)) +
  geom_col() + 
  facet_wrap(~EconomicEmpowerment) + 
  theme_bw()
  

EconomicEmpowermentData %>% 
  ggplot(aes(HHHLanguage,EconomicEmpowerment)) +
  geom_count()



