# This module is used to calculate three indicators, i.e., Percentage of farmers reporting increase in the production of rice,
# average post harvest losses and the total household income from rice production. These indicators will be calculated only for 
# the farmers who have grown rice (any type) in the last 12 months.

# Loading required libraries
library(tidyverse)
library(labelled)
library(expss)
library(readxl)

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

## Merge the rice two rosters
SAMSRoster <- left_join(PSAMSRiceRoster, 
                        PSAMSHarvestRoster,
                        by = c("interview_key", "RiceType")) %>% 
  # Aggregate Non-Organic rice production at individual household level
  group_by(interview_key, RiceType) %>%
  summarise(total_production = sum(PSAMSPHLCommQuant),.groups = "drop") %>% 
  left_join(PSAMSRiceRoster, by = c("interview_key", "RiceType")) %>% 
  left_join(PSAMSHarvestRoster, by = c("interview_key", "RiceType")) %>% 
  # Mutate vPSAMSRiceSellQuant,PSAMSRiceSellQuant, PSAMSRiceSellMN, PSAMSRiceInputsMN to be numeric
  mutate(PSAMSRiceSellQuant = as.numeric(PSAMSRiceSellQuant),
         PSAMSRiceSellMN = as.numeric(PSAMSRiceSellMN),
         PSAMSRiceInputsMN = as.numeric(PSAMSRiceInputsMN),
         PSAMSPHLCommArea = as.numeric(PSAMSPHLCommArea))


# Load in the household full roster data
HHFullAgricRoster <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select the necessary variables for this analysis
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage) %>% 
  # Distinct the data
  distinct(interview_key, .keep_all = TRUE)

## Calculate income from rice production
RiceIncomeRoster <- SAMSRoster %>% 
  distinct(interview_key, RiceType, .keep_all = TRUE) %>% 
  select(interview_key, RiceType, total_production, PSAMSPHLCommQuant, PSAMSPHLCommArea, PSAMSNutCropIncr,
         PSAMSRiceSellQuant, PSAMSPHLCommArea_Unit, PSAMSRiceSellMN, PSAMSRiceInputsMN, PSAMSPHLCommQntHand, PSAMSPHLCommQntLost) %>%
  # Mutate rice revenue
  mutate(PSAMSRiceRevenue = if_else(RiceType == "Non Organic Rice", total_production * 1096, total_production * 1106)) %>% 
  mutate(PSAMSPHLCommArea = if_else(PSAMSPHLCommArea_Unit == "Acre", PSAMSPHLCommArea * 0.01, PSAMSPHLCommArea)) %>%
  mutate(PSAMSPHLCommArea = if_else(PSAMSPHLCommArea_Unit == "Square Meter", PSAMSPHLCommArea * 0.0001, PSAMSPHLCommArea)) %>%
  # Mutate PSAMSPHLCommQntHand, PSAMSPHLCommQntLost to be numeric
  mutate(PSAMSPHLCommQntHand = as.numeric(PSAMSPHLCommQntHand),
         PSAMSPHLCommQntLost = as.numeric(PSAMSPHLCommQntLost)) %>%
  # Mutate PSAMSRiceIncome
  mutate(PSAMSRiceIncome = PSAMSRiceRevenue - PSAMSRiceInputsMN) %>% 
  # Change this income to USD
  mutate(PSAMSRiceIncome = PSAMSRiceIncome / 4100) 

# Merge the HHFullAgricRoster with the RiceIncomeRoster
HHFullAgricRoster <- left_join(HHFullAgricRoster, RiceIncomeRoster, by = "interview_key") %>% 
  drop_na(RiceType)

####################################ICOME FROM RICE PRODUCTION############################################

# Calculate average and median rice income for the total farmers
HHFullAgricRoster %>% 
  group_by(RiceType) %>% 
  summarise(AvgRiceIncome = mean(PSAMSRiceIncome, na.rm = TRUE),
            MedianRiceIncome = median(PSAMSRiceIncome, na.rm = TRUE))

# Calculate average rice production per hectare
HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>% 
  group_by(RiceType) %>%
  summarise(AvgRiceProduction = mean(AvgRiceProduction, na.rm = TRUE))

# Income by gender of household head
HHFullAgricRoster %>% 
  group_by(HHHSex, RiceType) %>% 
  summarise(AvgRiceIncome = mean(PSAMSRiceIncome, na.rm = TRUE),
            MedianRiceIncome = median(PSAMSRiceIncome, na.rm = TRUE))

# Income by Ethnicity of household head
HHFullAgricRoster %>% 
  group_by(HHHEthnicity, RiceType) %>% 
  summarise(AvgRiceIncome = mean(PSAMSRiceIncome, na.rm = TRUE),
            MedianRiceIncome = median(PSAMSRiceIncome, na.rm = TRUE))

####################################POST HARVEST LOSSES####################################################
# Calculate average post harvest losses

HHFullAgricRoster %>% 
  mutate(AvgPostHarvestLoss = PSAMSPHLCommQntLost / PSAMSPHLCommQntHand) %>% 
  group_by(RiceType) %>% 
  summarise(AvgPostHarvestLoss = mean(AvgPostHarvestLoss, na.rm = TRUE))

####################################INCREASE IN RICE PRODUCTION####################################################

# Calculate the percentage of farmers reporting increase in rice production
HHFullAgricRoster %>% 
  group_by(PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)

# Calculate the percentage of farmers reporting increase in rice production, disagregated by gender of the household heard
HHFullAgricRoster %>% 
  group_by(PSAMSNutCropIncr, HHHSex) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)





  