# This module is used to calculate three indicators, i.e., Percentage of farmers reporting increase in the production of rice,
# average post harvest losses and the total household income from rice production. These indicators will be calculated only for 
# the farmers who have grown rice (any type) in the last 12 months.

# Loading required libraries
library(tidyverse)
library(labelled)
library(expss)
library(readxl)

# Import first roster data
SAMSRoster1 <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx", 
    sheet = "Roster_PSAMSRice") %>% 
  # Selecting required columns
  select(interview__key, interview__id, Roster_PSAMSRice__id, 
         PSAMSRiceHarvestsNmb, PSAMSNutCropIncr, PSAMSPHLCommEnough,
         PSAMSRiceInputsMN, PSAMSRiceSell, PSAMSRiceSellTime, PSAMSRiceSellQuant,
         PSAMSRiceSellMN, PSAMSRiceRevenue, PSAMSRiceIncome, Income)


# Import second roster data
SAMSRoster2 <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx", 
    sheet = "Roster_Q15_21HarvestNmb") %>% 
  # Selecting required columns
  select(interview__key, interview__id, Roster_PSAMSRice__id, Roster_HarvestNmb__id, 
         PSAMSPHLCommArea, PSAMSPHLCommArea_Unit, PSAMSPHLCommArea_Unit_OTH, PSAMSPHLCommQuant,
         PSAMSPHLCommQntHand, PSAMSPHLCommQntLost)

# Import the data with other relevant variables from the household data set
HHLevelData <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>% 
  # Selecting required columns
  select(interview__key, interview__id, HHID, ADMIN4Name, ACName, HHBaseline, HHList, HHHSex,
         HHHEducation, HHHEthnicity, HHHLanguage, IDPoor, HHIncTot, contains("SAMSPHL")) %>% 
  # Pivot longer using PSAMSPHLCommN_1 and PSAMSPHLCommN_2 variables
  pivot_longer(cols = c("PSAMSPHLCommN__1", "PSAMSPHLCommN__2"), 
               names_to = "RiceType", 
               values_to = "Produced") %>%
  # Mutate the RiceType variable to be more descriptive
  mutate(RiceType = case_when(RiceType == "PSAMSPHLCommN__1" ~ "Organic Rice",
                              RiceType == "PSAMSPHLCommN__2" ~ "Non Organic Rice")) %>%
  # Mutate the Produced variable to be more descriptive
  mutate(Produced = case_when(Produced == 1 ~ "Yes",
                              TRUE ~ "No")) %>%
  # Filter out the rows where the Produced variable is "Yes"
  filter(Produced == "Yes")

# Join the HHLevelData with the first and second roster data using the interview__key variable


