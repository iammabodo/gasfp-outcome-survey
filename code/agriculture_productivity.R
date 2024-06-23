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


# Import the data with other relevant variables from the household data set
HHLevelData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key, HHID, ADMIN4Name, ACName, HHBaseline, HHList, SEX_HHH, SEX_Resp,
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
    RespSex = case_when(
    SEX_Resp == 0 ~ "Female",
    SEX_Resp == 1 ~ "Male"),
    HHHSex = case_when(
    SEX_HHH == 0 ~ "Female",
    SEX_HHH == 1 ~ "Male"), 
    HHHSex = if_else(is.na(HHHSex), RespSex, HHHSex),
    HHIncTot = as.numeric(HHIncTot)) %>% 
  select(-c(SEX_Resp, SEX_HHH)) %>% 
  # # Pivot longer using PSAMSPHLCommN_1 and PSAMSPHLCommN_2 variables
   pivot_longer(cols = c("PSAMSPHLCommN_1", "PSAMSPHLCommN_2"),
                names_to = "RiceType",
                values_to = "Produced") %>%
   # Mutate the RiceType variable to be more descriptive
   mutate(RiceType = case_when(RiceType == "PSAMSPHLCommN_1" ~ "Organic Rice",
                              RiceType == "PSAMSPHLCommN_2" ~ "Non Organic Rice")) %>%
  # Mutate the Produced variable to be more descriptive
    mutate(Produced = case_when(Produced == 1 ~ "Yes",
                             TRUE ~ "No")) %>%
  # Filter out the rows where the Produced variable is "Yes"
    #filter(Produced == "Yes") %>% 
  # Select only unique values of the interview_key
  # distinct(interview_key, .keep_all = TRUE) %>% 
  # Merge with TotalIncome data
  left_join(TotalIncome, by = "interview_key") %>%
  # Join with the HHDisabRoster data
  left_join(HHDisabRoster, by = "interview_key") %>%
  # Calculate rice income in USD
  mutate(RiceIncomeUSD = round((TotalIncome / 4100),2))

## Calculate the percentage of farmers reporting increase in the production of rice (Indicator 1)
# 1. Non Organic Rice
SAMSRoster %>% 
  filter(RiceType == "Non Organic Rice") %>%
  group_by(PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More")

# 2. Organic Rice
SAMSRoster %>% 
  filter(RiceType == "Organic Rice") %>%
  group_by(PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More")

# 3. All Rice Types
SAMSRoster %>% 
  group_by(PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(PSAMSNutCropIncr == "More")


## Calculating the average post harvest losses (Indicator 2)

# Calculate the average post harvest losses per individual farmer
AverageLossperfarmer <- PSAMSHarvestRoster %>% 
  group_by(interview_key) %>% 
  group_by(RiceType) %>%
  summarise(AvgPostHarvestLosses = mean(PSAMSPHLCommQntLostPerc,
                                        na.rm = TRUE))


# Calculate the average post harvest losses for all the farmers
AverageLossperfarmer %>% 
  summarise(AvgPostHarvestLosses = mean(AvgPostHarvestLosses,
                                        na.rm = TRUE))


# Calculate the total household income from rice production (Indicator 3). ## Please update this code here

SAMSRoster <- left_join(PSAMSRiceRoster, 
                        PSAMSHarvestRoster,
                        by = c("interview_key", "RiceType")) %>% 
  # Mutate the revenue from rice production
  mutate(NewPSAMSRiceRevenue = if_else(
    RiceType == "Organic Rice", PSAMSPHLCommQuant * 1106, PSAMSPHLCommQuant * 1096)) %>% 
  # Mutate the income from rice production
  mutate(NewPSAMSRiceIncome = NewPSAMSRiceRevenue - PSAMSRiceInputsMN) 

SAMSRoster %>% 
  # groub by rice type
  group_by(RiceType) %>%
  # Calcualte the average rice produced
  summarise(AvgRiceProduced = mean(PSAMSPHLCommQuant,
                                   na.rm = TRUE),
            N = n())

# Calculate the total household income from rice production
TotalIncome <- SAMSRoster %>% 
  group_by(interview_key) %>% 
  summarise(TotalIncome = sum(NewPSAMSRiceIncome,
                              na.rm = TRUE))


# Calculate mean income from rice production per household
HHLevelData %>% 
  group_by(RiceType) %>%
  summarise(MeanIncome = mean(TotalIncome,
                              na.rm = TRUE))

# Calculate the mean household income from rice production
HHLevelData %>% 
  group_by(RiceType) %>%
  summarise(MeanIncome = mean(NewPSAMSRiceIncome,
                              na.rm = TRUE))

HHSAMSRosterData <- HHLevelData %>% 
  # join with the SAMSRoster data
  left_join(SAMSRoster, by = c("interview_key", "RiceType")) %>%
  # Join with the TotalIncome data
  left_join(TotalIncome, by = "interview_key") %>% 
  select(-c(Roster_PSAMSRice_id.y, TotalIncome.y, Roster_PSAMSRice_id.x, TotalIncome.x))

HHSAMSRosterData %>% 
  # Group by RiceType
  group_by(RiceType.y) %>%
  # Calculate the mean income from rice production
  summarise(MeanIncome = mean(NewPSAMSRiceIncome,
                              na.rm = TRUE),
            N = n()) %>% 
  # Change the income to USD
  mutate(MeanIncomeUSD = MeanIncome / 4100)

# Calculate average rice output by rice type
SAMSRoster %>% 
  group_by(RiceType) %>% 
  summarise(AvgRiceProduced = mean(PSAMSPHLCommQuant,
                                   na.rm = TRUE),
            N = n())


# Calculate agricultural land area under farming by rice type
PSAMSHarvestRoster %>% 
  group_by(RiceType) %>% 
  summarise(AvgLandArea = mean(PSAMSPHLCommArea,
                              na.rm = TRUE),
            TotalLandArea = sum(PSAMSPHLCommArea, na.rm = TRUE),
            N = n())



