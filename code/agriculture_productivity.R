# This module is used to calculate three indicators, i.e., Percentage of farmers reporting increase in the production of rice,
# average post harvest losses and the total household income from rice production. These indicators will be calculated only for 
# the farmers who have grown rice (any type) in the last 12 months.

# Loading required libraries
library(tidyverse)
library(labelled)
library(expss)
library(readxl)
2. 
# Import first roster data
PSAMSRiceRoster <- read_excel("data/Roster_PSAMSRice_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key,  Roster_PSAMSRice_id, 
         PSAMSRiceHarvestsNmb, PSAMSNutCropIncr, PSAMSPHLCommEnough,
         PSAMSRiceInputsMN, PSAMSRiceSell, PSAMSRiceSellTime, PSAMSRiceSellQuant,
         PSAMSRiceSellMN, PSAMSRiceRevenue, PSAMSRiceIncome, Income) %>% 
  # Mutate variables to have more descriptive values
  mutate(PSAMSNutCropIncr = case_when(
    PSAMSNutCropIncr == 1 ~ "More",
    PSAMSNutCropIncr == 2 ~ "Less",
    PSAMSNutCropIncr == 3 ~ "The Same",
    TRUE ~ "Not Applicable"),
    PSAMSPHLCommEnough = case_when(
    PSAMSPHLCommEnough == 1 ~ "Yes",
    TRUE ~ "No"),
    PSAMSRiceSell = case_when(
    PSAMSRiceSell == 1 ~ "Yes",
    PSAMSRiceSell == 0 ~ "No",
    TRUE ~ "Don't Know"),
    RiceType = case_when(
    Roster_PSAMSRice_id == 1 ~ "Organic Rice",
    Roster_PSAMSRice_id == 2 ~ "Non Organic Rice"))

# Import second roster data
PSAMSHarvestRoster <- read_excel("data/Roster_HarvestNumb_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key, Roster_PSAMSRice_id, Roster_HarvestNmb_id, 
         PSAMSPHLCommArea, PSAMSPHLCommArea_Unit, PSAMSPHLCommArea_Unit_OTH, PSAMSPHLCommQuant,
         PSAMSPHLCommQntHand, PSAMSPHLCommQntLost) %>% 
  # Rename PSAMSPHLCommArea_Unit to have more descriptive values
  mutate(PSAMSPHLCommArea_Unit = case_when(
    PSAMSPHLCommArea_Unit == 1 ~ "Square Meter",
    PSAMSPHLCommArea_Unit == 2 ~ "Acre",
    PSAMSPHLCommArea_Unit == 3 ~ "Kong",
    PSAMSPHLCommArea_Unit == 4 ~ "Hectare",
    TRUE ~ "Other"),
    RiceType = case_when(
    Roster_PSAMSRice_id == 1 ~ "Organic Rice",
    Roster_PSAMSRice_id == 2 ~ "Non Organic Rice"))

# Import the data with other relevant variables from the household data set
HHLevelData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key, HHID, ADMIN4Name, ACName, HHBaseline, HHList, SEX_HHH,
         HHHEducation, HHHEthnicity, HHHLanguage, IDPoor, HHIncTot, contains("SAMSPHL")) %>% 
  # Rename ADMIN4Name, ACName, HHBaseline, HHHEducation, HHHEthnicity, HHHLanguage, IDPoor and HHHSex to have more descriptive values
  mutate(ADMIN4Name = case_when(
    ADMIN4Name == 100 ~ "Nang Khi Loek",
    ADMIN4Name == 200 ~ "Ou Buon Leu",
    ADMIN4Name == 300 ~ "Roya",
    ADMIN4Name == 400 ~ "Sokh Sant",
    ADMIN4Name == 500 ~ "Srae Huy",
    ADMIN4Name == 600 ~ "Srae Sangkom",
    TRUE ~ "Other"),
    ACName = case_when(
    ACName == 1 ~ "Phum Srae Huy",
    ACName == 2 ~ "Samaki Mean Rith Rung Roeung",
    ACName == 3 ~ "Samaki Phum Toul",
    ACName == 4 ~ "Apiwat Mean Chey",
    ACName == 5 ~ "Samaki Rik Chom Roeun"),
    HHBaseline = case_when(
    HHBaseline == 1 ~ "Baseline Members",
    HHBaseline == 0 ~ "New Members",
    TRUE ~ "Don't Know"),
    HHHEducation = case_when(
    HHHEducation == 1 ~ "No Schooling",
    HHHEducation == 2 ~ "Some Pre-Primary",
    HHHEducation == 3 ~ "Some Primary",
    HHHEducation == 4 ~ "Completed Primary",
    HHHEducation == 5 ~ "Some Secondary",
    HHHEducation == 6 ~ "Completed Secondary",
    HHHEducation == 7 ~ "Some High School",
    HHHEducation == 8 ~ "Completed High School",
    HHHEducation == 9 ~ "Vocational",
    HHHEducation == 10 ~ "Some University",
    HHHEducation == 11 ~ "Completed University",
    TRUE ~ "Don't Know"),
    HHHEthnicity = case_when(
    HHHEthnicity == 3 | HHHEthnicity == 11 | HHHEthnicity == 12  ~ "Ethnic Minority",
    HHHEthnicity == 999 ~ "Other",
    HHHEthnicity == 888 ~ "Don't Know / prefer not to answer",
    TRUE ~ "Indigenous"),
    HHHLanguage = case_when(
    HHHLanguage == 1 ~ "Khmer",
    HHHLanguage == 2 ~ "Bunong",
    TRUE ~ "Other"),
    IDPoor = case_when(
    IDPoor == 1 ~ "IDPoor",
    IDPoor == 0 | IDPoor == 2 ~ "Not IDPoor",
    IDPoor == 888 ~ "Don't Know",
    TRUE ~ "Refuse / prefer not to answer"),
    HHIncTot = as.numeric(HHIncTot)) %>% 
  # Pivot longer using PSAMSPHLCommN_1 and PSAMSPHLCommN_2 variables
  pivot_longer(cols = c("PSAMSPHLCommN_1", "PSAMSPHLCommN_2"), 
               names_to = "RiceType", 
               values_to = "Produced") %>%
  # Mutate the RiceType variable to be more descriptive
  mutate(RiceType = case_when(RiceType == "PSAMSPHLCommN_1" ~ "Organic Rice",
                              RiceType == "PSAMSPHLCommN_2" ~ "Non Organic Rice")) %>%
  # Mutate the Produced variable to be more descriptive
  mutate(Produced = case_when(Produced == 1 ~ "Yes",
                              TRUE ~ "No")) #%>%
  # Filter out the rows where the Produced variable is "Yes"
  #filter(Produced == "Yes")

## Joining the three data sets

# Join the SAMSRoster1 and SAMSRoster2 data sets
SAMSRoster <- left_join(SAMSRoster1, 
                        SAMSRoster2, 
                        by = c("interview_key", "RiceType"))

# Join the SAMSRoster and HHLevelData data sets
HHSAMSRoster <- left_join(HHLevelData, 
                          SAMSRoster,
                          by = c("interview_key", "RiceType"))

## Calculate the indicators

# 1. Calculate the percentage of farmers reporting increase in the production of rice
HHSAMSRoster %>% 
  group_by(RiceType, PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)

# 2. Calculate the average post harvest losses. This code for indicator need to be revised
HHSAMSRoster %>% 
  mutate(PSAMSPHLCommQntLost = as.numeric(PSAMSPHLCommQntLost),
         PSAMSPHLCommQuant = as.numeric(PSAMSPHLCommQuant)) %>%
  # Calculate the percentage of post harvest losses
  mutate(PSAMSPHLCommQntLost = (PSAMSPHLCommQntLost / PSAMSPHLCommQuant) * 100) %>%
  # Mutate average loss per farmer
  group_by(HHID) %>%
  mutate(averagelossperfarmer = mean(PSAMSPHLCommQntLost,
                                     na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(HHHEthnicity, IDPoor, HHHLanguage) %>%
  summarise(AvgPostHarvestLosses = mean(averagelossperfarmer,
                                        na.rm = TRUE))

# 3. Calculate the total household income from rice production
HHSAMSRoster %>% 
  # Mutate rice income variable
  #Convert PSAMSPHLCommQuant, PSAMSRiceSellMN and PSAMSRiceInputsMN to numeric variables
  mutate(PSAMSPHLCommQuant = as.numeric(PSAMSPHLCommQuant),
         PSAMSRiceSellMN = as.numeric(PSAMSRiceSellMN),
         PSAMSRiceInputsMN = as.numeric(PSAMSRiceInputsMN)) %>%
  mutate(RiceIncome = (PSAMSPHLCommQuant * PSAMSRiceSellMN) - PSAMSRiceInputsMN) %>%
  group_by(HHHEthnicity, IDPoor, HHHLanguage) %>% 
  summarise(TotalHouseholdIncome = sum(RiceIncome,
                                       na.rm = TRUE))

