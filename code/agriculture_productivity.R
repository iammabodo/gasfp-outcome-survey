# This module is used to calculate three indicators, i.e., Percentage of farmers reporting increase in the production of rice,
# average post harvest losses and the total household income from rice production. These indicators will be calculated only for 
# the farmers who have grown rice (any type) in the last 12 months.

# Loading required libraries
library(tidyverse)
library(labelled)
library(expss)
library(readxl)

# Import first roster data.
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
HHFullDemographicRoster <- read_excel("data/FullHHRosterClean.xlsx") %>% 
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
  group_by(interview_key, RiceType) %>%
  mutate(PSAMSRiceRevenue = sum(PSAMSRiceRevenue, na.rm = TRUE)) %>%
  # Mutate PSAMSRiceIncome
  mutate(PSAMSRiceIncome = PSAMSRiceRevenue - PSAMSRiceInputsMN) %>% 
  # Change this income to USD
  mutate(PSAMSRiceIncome = PSAMSRiceIncome / 4100) %>%
  # Round the income to 2 decimal places
  mutate(PSAMSRiceIncome = round(PSAMSRiceIncome, 2))

# Merge the HHFullAgricRoster with the RiceIncomeRoster
HHFullAgricRoster <- left_join(HHFullAgricRoster, RiceIncomeRoster, by = "interview_key") %>% 
  drop_na(RiceType)

####################################ICOME FROM RICE PRODUCTION############################################

data <- HHFullAgricRoster %>%
  group_by(interview_key) %>%
  mutate(RiceProduced = case_when(
    any(RiceType == "Organic Rice") & any(RiceType == "Non Organic Rice") ~ "Both",
    all(RiceType == "Organic Rice") ~ "Organic Rice",
    all(RiceType == "Non Organic Rice") ~ "Non Organic Rice"
  )) %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  select(interview_key, RiceProduced, RiceType)

# Calculate average and median rice income for the total farmers
RiceIncome <- HHFullAgricRoster %>% 
  group_by(interview_key) %>% 
  summarise(TotalRiceIncome = sum(PSAMSRiceIncome, na.rm = TRUE)) %>% 
  ungroup() %>%
  # Merge with data to get the rice type
  left_join(data, by = "interview_key")

# Join the HHFullDemographicRoster with the RiceIncome
HHFullAgricIncomeRoster <- left_join(HHFullDemographicRoster, RiceIncome, by = "interview_key") %>% 
  drop_na(RiceProduced) %>% 
  #Mutate every character variable to be a factor
  mutate_if(is.character, as.factor)

# Calculate income from Rice Production
IncomeFromRiceProduction <- HHFullAgricIncomeRoster %>% 
  summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(RiceIncome = round(RiceIncome, 2)) %>% 
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, RiceIncome)

# Calculate midean income disaggregated by the gender of the household heard
IncomeByGender <- HHFullAgricIncomeRoster %>% 
  group_by(HHHSex) %>% 
  summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(RiceIncome = round(RiceIncome, 2)) %>% 
  rename(Disagregation = HHHSex)


# Calculate midean income disaggregated by HHHEthnicity
IncomeByEthnicity <- HHFullAgricIncomeRoster %>% 
  group_by(HHHEthnicity) %>% 
  summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(RiceIncome = round(RiceIncome, 2)) %>% 
  filter(HHHEthnicity != "Foreigners") %>% 
  rename(Disagregation = HHHEthnicity)

# Calculate midean income disaggregated by RiceProduced
IncomeByRiceProduced <- HHFullAgricIncomeRoster %>% 
  group_by(RiceProduced) %>% 
  summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(RiceIncome = round(RiceIncome, 2)) %>% 
  rename(Disagregation = RiceProduced)

# Combine the tables
IncomeFromRiceProduction <- bind_rows(IncomeFromRiceProduction, IncomeByGender, IncomeByEthnicity, IncomeByRiceProduced) %>% 
  mutate_if(is.character, as.factor)

# Write the table to an excel file
write.xlsx(IncomeFromRiceProduction, "report/IncomeFromRiceProduction.xlsx")
####################################POST HARVEST LOSSES####################################################
# Calculate average post harvest losses

PHLosses <- HHFullAgricRoster %>% 
  group_by(interview_key, RiceType) %>%
  mutate(perc_loss = round((PSAMSPHLCommQntLost /(PSAMSPHLCommQntHand) * 100),1))

HHFullPHLAgricRoster <- HHFullAgricRoster %>% 
  select(interview_key, ADMIN4Name, ACName, HHBaseline, RiceType, IDPoor, HHHSex, RespSex, HHHEthnicity,) %>% 
  left_join(PHLosses, by = c("interview_key", "RiceType")) %>% 
  drop_na(RiceType)

####################################INCREASE IN RICE PRODUCTION####################################################

# Calculate the percentage of farmers reporting increase in rice production
HHFullAgricRoster %>% 
  group_by(PSAMSNutCropIncr, RiceType) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More")

# Calculate the percentage of farmers reporting increase in rice production, disagregated by gender of the household heard
HHFullAgricRoster %>% 
  group_by(PSAMSNutCropIncr, HHHSex) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More")

# Calculate the percentage of farmers reporting increase in rice production, disagregated by ethnicity of the household heard
HHFullAgricRoster %>% 
  group_by(PSAMSNutCropIncr, HHHEthnicity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More" & HHHEthnicity != "Foreigners")


#############################################RICE PRODUCTION####################################################
# Calculate the total rice production per hectare
ProductivityByRiceType <- HHFullAgricRoster %>% 
  group_by(RiceType) %>% 
  summarise(TotalRiceProduction = mean(total_production, na.rm = TRUE)) %>% 
  mutate(TotalRiceProduction = round(TotalRiceProduction, 2))


ProductivityByGender <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType, HHHSex) %>%
  summarise(AvgRiceProduction = mean(AvgRiceProduction, na.rm = TRUE))

ProductivityByEthnicity <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType, HHHEthnicity) %>%
  summarise(AvgRiceProduction = mean(AvgRiceProduction, na.rm = TRUE)) %>% 
  filter(HHHEthnicity != "Foreigners")



##############################################END OF SCRIPT####################################################


  