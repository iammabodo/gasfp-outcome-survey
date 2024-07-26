# This module is used to calculate three indicators, i.e., Percentage of farmers reporting increase in the production of rice,
# average post harvest losses and the total household income from rice production. These indicators will be calculated only for 
# the farmers who have grown rice (any type) in the last 12 months.

# Loading required libraries
library(tidyverse)
library(labelled)
library(expss)
library(readxl)
library(openxlsx)
library(interactions)

# Import first roster data. To be used for calculation of indicator 1. Households reporting increase in the production of rice and Income
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
    Roster_PSAMSRice_id == 2 ~ "Non Organic Rice")) %>% 
  # Mutate PSAMSRiceIncome, PSAMSRiceSellQuant, Income to be numeric
  mutate(PSAMSRiceIncome = as.numeric(PSAMSRiceIncome),
         PSAMSRiceSellQuant = as.numeric(PSAMSRiceSellQuant),
         Income = as.numeric(Income),
         PSAMSRiceInputsMN = as.numeric(PSAMSRiceInputsMN)) %>% 
  # Join with the HHcharacteristics data
  left_join(HHFullDemographicRoster, by = "interview_key")

# Small Roster Data for Calculation of Income from Rice Production
SmallRiceRoster <- PSAMSRiceRoster %>% 
  filter(PSAMSRiceSell == "Yes") 

# Lets remove Outliers from the data
summary_stats <- SmallRiceRoster %>%
  group_by(interview_key) %>%
  mutate(TotalIncome = sum(Income, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  summarise(
    Q1 = quantile(TotalIncome, 0.25, na.rm = TRUE),
    Q3 = quantile(TotalIncome, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR
  )


## Calculate the average, median quantity of rice sold by the farmers, ethnicity and ricetype
RiceSold <- SmallRiceRoster %>% 
  group_by(HHHEthnicity, RiceType) %>% 
  summarise(MedianQuantity = median(PSAMSRiceSellQuant, na.rm = TRUE),
            AverageQuantity = mean(PSAMSRiceSellQuant, na.rm = TRUE),
            Farmers = n()) %>% 
  ungroup() %>% 
  filter(!is.na(HHHEthnicity)) %>% 
  #Convert these to metric tonnes
  mutate(MedianQuantity = round((MedianQuantity / 1000),2),
         AverageQuantity = round((AverageQuantity / 1000),2))

#Write a xlsx file
write.xlsx(RiceSold, "report/RiceSold.xlsx")


# Import second roster data. Essential for the calculation of Post Harvest Loses
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


# Load in the household full roster data. This is essential for dis aggregations

HHFullDemographicRoster <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select the necessary variables for this analysis
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage) %>% 
  # Distinct the data
  distinct(interview_key, .keep_all = TRUE)

######################################################RICE INCOME CALCULATION (Realistic Estimation)###################################################
## Calculate income from rice production. This is significantly different from the baseline calculation
## It gives a more realistic estimation of the income from rice production, as advised by the World Bank

RiceIncomeRoster <- SAMSRoster %>%
  distinct(interview_key, RiceType, .keep_all = TRUE) %>%
  select(interview_key, RiceType, total_production, PSAMSPHLCommQuant, PSAMSPHLCommArea, PSAMSNutCropIncr,
         PSAMSRiceSellQuant, PSAMSPHLCommArea_Unit, PSAMSRiceSellMN, PSAMSRiceInputsMN, PSAMSPHLCommQntHand, PSAMSPHLCommQntLost) %>%
  # Mutate rice revenue
  mutate(PSAMSRiceRevenue = if_else(RiceType == "Non Organic Rice", total_production * 1100, total_production * 1200)) %>%
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

# Merge the HHFullAgricRoster with the RiceIncomeRoster. To be used for calculation of indicator 3.
# Total household income from rice production
HHFullAgricRoster <- left_join(HHFullDemographicRoster, RiceIncomeRoster, by = "interview_key") %>%
  drop_na(RiceType)

Q1 <- quantile(HHFullAgricRoster$total_production, 0.25)
Q3 <- quantile(HHFullAgricRoster$total_production, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

HHFullAgricRoster <- HHFullAgricRoster %>%
  filter(total_production >= lower_bound & total_production <= upper_bound)

####################################ICOME FROM RICE PRODUCTION############################################
# USE THIS SCRIPT TO CALCULATE THE INCOME FROM RICE PRODUCTION USING THE MORE REALISTIC WAY, IF AGREED UPON BY THE TEAM. OTHREWISE, USE THE OLD WAY
# Mutate the variable  to indicate which rice types did the farmer produce (i.e., Organic, Non-Organic or Both)
# This is not a requirement, but a necessity in case we want to see how income is different among these three groups
# Determine the type of rice produced by the farmer
RiceProductionData <- HHFullAgricRoster %>%
  group_by(interview_key) %>%
  mutate(RiceProduced = case_when(
    any(RiceType == "Organic Rice") & any(RiceType == "Non Organic Rice") ~ "Both",
    all(RiceType == "Organic Rice") ~ "Organic Rice",
    all(RiceType == "Non Organic Rice") ~ "Non Organic Rice"
  )) %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  select(interview_key, RiceProduced, RiceType)

write.xlsx(RiceProductionData, "data/RiceProduced.xlsx")
# 
# # Calculate average and median rice income for the total farmers (Version 1)
# RiceIncome <- HHFullAgricRoster %>% 
#   group_by(interview_key) %>% 
#   summarise(TotalRiceIncome = sum(PSAMSRiceIncome, na.rm = TRUE)) %>% 
#   ungroup() %>%
#   # Merge with data to get the rice type
#   left_join(RiceProductionData, by = "interview_key")
# 
# # Join the HHFullDemographicRoster with the RiceIncome.
# # This join is important such that we can have the disagregations in the indicator calculation
# 
# HHFullAgricIncomeRoster <- left_join(HHFullDemographicRoster, RiceIncome, by = "interview_key") %>% 
#   drop_na(RiceProduced) %>% 
#   #Mutate every character variable to be a factor
#   mutate_if(is.character, as.factor)
# 
# 
# # Calculate income from Rice Production
# IncomeFromRiceProduction <- HHFullAgricIncomeRoster %>% 
#   summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
#   mutate(RiceIncome = round(RiceIncome, 2)) %>% 
#   mutate(Disagregation = "Total") %>% 
#   select(Disagregation, RiceIncome)
# 
# # Calculate midean income disaggregated by the gender of the household heard
# IncomeByGender <- HHFullAgricIncomeRoster %>% 
#   group_by(HHHSex) %>% 
#   summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
#   mutate(RiceIncome = round(RiceIncome, 2)) %>% 
#   rename(Disagregation = HHHSex)
# 
# 
# # Calculate median income disaggregated by HHHEthnicity
# IncomeByEthnicity <- HHFullAgricIncomeRoster %>% 
#   group_by(HHHEthnicity) %>% 
#   summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
#   mutate(RiceIncome = round(RiceIncome, 2)) %>% 
#   filter(HHHEthnicity != "Foreigners") %>% 
#   rename(Disagregation = HHHEthnicity)
# 
# # Calculate midean income disaggregated by RiceProduced
# IncomeByRiceProduced <- HHFullAgricIncomeRoster %>% 
#   group_by(RiceProduced) %>% 
#   summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
#   mutate(RiceIncome = round(RiceIncome, 2)) %>% 
#   rename(Disagregation = RiceProduced)
# 
# # Calculate midean income disaggregated by IDPoor
# IncomeByIDPoor <- HHFullAgricIncomeRoster %>% 
#   group_by(IDPoor) %>% 
#   summarise(RiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
#   mutate(RiceIncome = round(RiceIncome, 2)) %>% 
#   rename(Disagregation = IDPoor)
# 
# # Combine the tables
# IncomeFromRiceProduction <- bind_rows(IncomeFromRiceProduction, IncomeByGender, IncomeByEthnicity, IncomeByRiceProduced, IncomeByIDPoor) %>% 
#   mutate_if(is.character, as.factor)
# 
# # Write the table to an excel file
# write.xlsx(IncomeFromRiceProduction, "report/IncomeFromRiceProduction.xlsx")


####################################POST HARVEST LOSSES####################################################
# Calculate average post harvest losses

PHLosses <- HHFullAgricRoster %>% 
  group_by(interview_key, RiceType) %>%
  mutate(perc_loss = round((PSAMSPHLCommQntLost /(PSAMSPHLCommQntHand) * 100),1)) %>% 
  ungroup() %>%
  group_by(interview_key) %>%
  mutate(AvgPHLoss = mean(perc_loss, na.rm = TRUE))

# Merge with the household roster to get the disagregations

Q1 <- quantile(data$value, 0.25)
Q3 <- quantile(data$value, 0.75)
IQR <- Q3 - Q1

HHFullPHLAgricRoster <- HHFullAgricRoster %>% 
  select(interview_key, ADMIN4Name, ACName, HHBaseline, RiceType, IDPoor, HHHSex, RespSex, HHHEthnicity) %>% 
  left_join(PHLosses, by = c("interview_key", "RiceType", "HHBaseline", "HHHSex", "RespSex", "HHHEthnicity", "IDPoor", "ADMIN4Name", "ACName")) %>% 
  left_join(RiceProductionData, by = c("interview_key", "RiceType")) %>% 
  drop_na(RiceType) %>%
  distinct(interview_key, .keep_all = TRUE)

# Calculate the average total post harvest losses indicator
TotalPHLosses <- HHFullPHLAgricRoster %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, AvgPHLoss)

# Calculate the average total post harvest losses indicator, disagaggregated by HHHSex
PHLossesByGender <- HHFullPHLAgricRoster %>% 
  group_by(HHHSex) %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  rename(Disagregation = HHHSex)

# Calculate the average total post harvest losses indicator, disagaggregated by HHHEthnicity
PHLossesByEthnicity <- HHFullPHLAgricRoster %>% 
  group_by(HHHEthnicity) %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  filter(HHHEthnicity != "Foreigners") %>% 
  rename(Disagregation = HHHEthnicity)

# Calculate the average total post harvest losses indicator, disagaggregated by RiceType
PHLossesByRiceType <- HHFullPHLAgricRoster %>% 
  group_by(RiceType) %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  rename(Disagregation = RiceType)

# Calculate the average total post harvest losses indicator, disagaggregated by IDPoor Status
PHLossesByIDPoor <- HHFullPHLAgricRoster %>% 
  group_by(IDPoor) %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  rename(Disagregation = IDPoor)

PHLossesByRiceProduced <- HHFullPHLAgricRoster %>% 
  group_by(RiceProduced) %>% 
  summarise(AvgPHLoss = mean(AvgPHLoss, na.rm = TRUE)) %>% 
  mutate(AvgPHLoss = round(AvgPHLoss, 2)) %>% 
  rename(Disagregation = RiceProduced)

# Combine the tables
PHLosses <- bind_rows(TotalPHLosses, PHLossesByGender, PHLossesByEthnicity, PHLossesByRiceType, PHLossesByIDPoor, PHLossesByRiceProduced) %>% 
  mutate_if(is.character, as.factor)

# Write the table to an excel file
write.xlsx(PHLosses, "report/PHLosses.xlsx")

####################################INCREASE IN RICE PRODUCTION####################################################

# Calculate the percentage of farmers reporting increase in rice production
TotalIncProduction <- HHFullPHLAgricRoster %>% 
  group_by(PSAMSNutCropIncr) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(PSAMSNutCropIncr == "More") %>% 
  mutate(Disagregation = "Total") %>%
  select(Disagregation, Percentage)

# Calculate the percentage of farmers reporting increase in rice production, disagregated by gender of the household heard
GenderIncProduction <- HHFullPHLAgricRoster %>% 
  group_by(PSAMSNutCropIncr, HHHSex) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(PSAMSNutCropIncr == "More") %>% 
  rename(Disagregation = HHHSex) %>%
  select(Disagregation, Percentage)

# Calculate the indicator disaggregated by ethnicity of the household heard
EthnicityIncProduction <- HHFullPHLAgricRoster %>% 
  group_by(PSAMSNutCropIncr, HHHEthnicity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(PSAMSNutCropIncr == "More" & HHHEthnicity != "Foreigners") %>% 
  rename(Disagregation = HHHEthnicity) %>%
  select(Disagregation, Percentage)

# Calculate the indicator disaggregated by IDPoor status
IDPoorIncProduction <- HHFullPHLAgricRoster %>% 
  group_by(PSAMSNutCropIncr, IDPoor) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(PSAMSNutCropIncr == "More") %>% 
  rename(Disagregation = IDPoor) %>% 
  select(Disagregation, Percentage)

RiceProducedIncProduction <- HHFullPHLAgricRoster %>% 
  group_by(PSAMSNutCropIncr, RiceProduced) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(PSAMSNutCropIncr == "More") %>% 
  rename(Disagregation = RiceProduced) %>% 
  select(Disagregation, Percentage)

## Merge the tables
IncProduction <- bind_rows(TotalIncProduction, GenderIncProduction, EthnicityIncProduction, IDPoorIncProduction, RiceProducedIncProduction) %>% 
  mutate_if(is.character, as.factor) %>% 
  #round the percentage to 2 decimal places
  mutate(Percentage = round(Percentage, 2))

# Write the table to an excel file
write.xlsx(IncProduction, "report/IncProduction.xlsx")

#############################################RICE PRODUCTION####################################################
# Calculate the average rice production per hectare
ProductivityByRiceType <- HHFullAgricRoster %>% 
  mutate(HectRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType) %>% 
  summarise(TotalRiceProduction = mean(HectRiceProduction, na.rm = TRUE)) %>% 
  mutate(TotalRiceProduction = round(TotalRiceProduction, 2))


ProductivityByGender <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType, HHHSex) %>%
  summarise(AvgRiceProduction = mean(AvgRiceProduction, na.rm = TRUE))

ProductivityByEthnicity <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType, HHHEthnicity) %>%
  summarise(AvgRiceProduction = mean(AvgRiceProduction, na.rm = TRUE),
            n = n()) 

Productivity <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(RiceType) %>%
  summarise(TotalRiceProduction = mean(AvgRiceProduction, na.rm = TRUE)) %>% 
  mutate(TotalRiceProduction = round(TotalRiceProduction, 2))

# Productivity by ADmin4Name
ProductivityByAdmin4Name <- HHFullAgricRoster %>% 
  mutate(AvgRiceProduction = total_production / PSAMSPHLCommArea) %>%
  group_by(ACName, RiceType, HHHEthnicity) %>% 
  summarise(TotalRiceProduction = mean(AvgRiceProduction, na.rm = TRUE),
            n = n()) %>% 
  mutate(TotalRiceProduction = round(TotalRiceProduction, 2)) %>% 
  filter(RiceType == "Organic Rice") %>% 
  filter(!is.na(HHHEthnicity))

HHFullAgricRoster %>% 
  count(ACName, RiceType, HHHEthnicity) %>%
  filter(HHHEthnicity == "Ethnic Minority")

##############################################END OF SCRIPT####################################################

# Join ECMEN Data and Rice Income data. THIS IS NOT A REQUIREMENT, BUT RATHER MAYBE USED FOR FURTHER ANALYSIS ON THE RELATIONSHIP BETWEEN RICE INCOME AND,
# TOTAL EXPENDITURE PER CAPITA, RICE INCOME PER CAPITA, AND RICE INCOME PER MONTH PER CAPITA

IncomeEconCapacityData <- ECMENdata %>%
   select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity,
          TotalExpPerCapitaUSD) %>%
   # Left journey with the RiceIncome data
   left_join(RiceIncome, by = "interview_key") %>%
   # Drop NAs for TotalRiceIncome and RiceProduced
   drop_na(TotalRiceIncome, RiceProduced) %>%
   distinct(interview_key, .keep_all = TRUE) %>%
   # Mutate RiceIncomePerCapita
   mutate(RiceIncomePerCapita = TotalRiceIncome / HHList,
          TotalYExpperCapitaUSD = TotalExpPerCapitaUSD * HHList,
          RiceMPerCapita = RiceIncomePerCapita / 12)

 IncomeEconCapacityData %>%
   group_by(RiceProduced, HHHEthnicity) %>%
   summarise(Coun = n(),
            meanRiceIncome = mean(TotalRiceIncome, na.rm = TRUE),
            meanTE = mean(TotalExpPerCapitaUSD, na.rm = TRUE)) %>%
   drop_na(HHHEthnicity)

################################################################CALCULATE RICE INCOME USING The Old Way############################################
RiceIncomeoutliers_summary_stats <- PSAMSRiceRoster %>%
  filter(PSAMSRiceSell == "Yes") %>%
  group_by(interview_key) %>%
  mutate(TotalRiceIncome = sum(Income, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  summarise(
    Q1 = quantile(TotalRiceIncome, 0.25, na.rm = TRUE),
    Q3 = quantile(TotalRiceIncome, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR
  )

# Calculate rice income per household

PSAMSRiceIncome <- PSAMSRiceRoster %>% 
  filter(PSAMSRiceSell == "Yes") %>%
  distinct(interview_key, RiceType, .keep_all = TRUE) %>%
  group_by(interview_key) %>% 
  summarise(TotalRiceIncome = sum(Income, na.rm = TRUE)) %>% 
  ungroup() %>%
  # Filtering outliers
  #filter(TotalRiceIncome >= RiceIncomeoutliers_summary_stats$LowerBound[1] & TotalRiceIncome <= RiceIncomeoutliers_summary_stats$UpperBound[1]) %>%
  # Merge with the PSAMSRiceRoster to get the rice type
  left_join(HHFullDemographicRoster, by = "interview_key") %>%
  left_join(RiceProductionData, by = "interview_key") %>%
  drop_na(HHHEthnicity)
 

# Calculate the total rice income by finding the median of the TotalRiceIncome
TotOldRiceIncome <- PSAMSRiceIncome %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, MedianRiceIncome) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

# Calculate the total rice income by finding the median of the TotalRiceIncome, after grouping by HHHSex
OldRiceIncomeByGender <- PSAMSRiceIncome %>% 
  filter(!is.na(HHHSex)) %>%
  group_by(HHHSex) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  rename(Disagregation = HHHSex) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

# Calculate the total rice income by finding the median of the TotalRiceIncome, after grouping by HHHEthnicity
OldRiceIncomeByEthnicity <- PSAMSRiceIncome %>% 
  group_by(HHHEthnicity) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  filter(HHHEthnicity != "Foreigners") %>% 
  rename(Disagregation = HHHEthnicity) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

# Calculate the total rice income by finding the median of the TotalRiceIncome, after grouping by rice produced
OldRiceIncomeByRiceProduced <- PSAMSRiceIncome %>% 
  filter(!is.na(RiceProduced)) %>%
  group_by(RiceProduced) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  rename(Disagregation = RiceProduced) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

OldRiceIncomeByRiceProducedHHE <- PSAMSRiceIncome %>% 
  filter(!is.na(RiceProduced)) %>%
  group_by(RiceProduced, HHHEthnicity) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE),
            n = n()) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  rename(Disagregation = RiceProduced) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

write.xlsx(OldRiceIncomeByRiceProducedHHE, "report/OldRiceIncomeByRiceProducedHHE.xlsx")

# Calculate the total rice income by finding the median of the TotalRiceIncome, after grouping by IDPoor
OldRiceIncomeByIDPoor <- PSAMSRiceIncome %>% 
  filter(!is.na(IDPoor)) %>%
  group_by(IDPoor) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE)) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  rename(Disagregation = IDPoor) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

# Combine the tables
OldRiceIncome <- bind_rows(TotOldRiceIncome, OldRiceIncomeByGender, OldRiceIncomeByEthnicity, OldRiceIncomeByRiceProduced, OldRiceIncomeByIDPoor) %>% 
  mutate_if(is.character, as.factor)


OldRiceIncomeByRiceProducedandEthnicity <- PSAMSRiceIncome %>% 
  group_by(RiceProduced, HHHEthnicity) %>% 
  summarise(MedianRiceIncome = median(TotalRiceIncome, na.rm = TRUE),
            n = n()) %>% 
  mutate(MedianRiceIncome = round(MedianRiceIncome, 2)) %>% 
  rename(Disagregation = RiceProduced) %>% 
  mutate(MedianRiceIncome = MedianRiceIncome / 4100)

# Write the table to an excel file
write.xlsx(OldRiceIncome, "report/OldRiceIncome.xlsx")

write.xlsx(OldRiceIncomeByRiceProducedandEthnicity, "report/OldRiceIncomeByRiceProducedandEthnicity.xlsx")
# Prices

RicePrices <- PSAMSRiceRoster %>% 
  filter(PSAMSRiceSell == "Yes" & !is.na(HHHEthnicity)) %>% 
  group_by(RiceType, HHHSex) %>% 
  summarise(medianPrice = median(as.numeric(PSAMSRiceSellMN), na.rm = T), 
            std = sd(as.numeric(PSAMSRiceSellMN), na.rm = T), 
            minP= min(as.numeric(PSAMSRiceSellMN), na.rm = T), 
            maxP = max(as.numeric(PSAMSRiceSellMN), na.rm = T), 
            n = n())


write.xlsx(RicePrices, "report/RicePrices.xlsx")

#################################################################ESTIMATING THE RELATIONSHIP BETWEEN HHEXP AND RICE INCOME############################################

# Modelling the relationship between rice income and total expenditure per capita. It is not a requirement though
# sub-setting the data to include the household expenditure variables

ShortECMEN <- ECMENdata %>%
  select(interview_key, TotalFoodExp, TotalNonFoodExp, TotalNonFoodIntExp, TotalExp, TotalExpPerCapita, TotalExpPerCapitaUSD) %>%
  distinct(interview_key, .keep_all = TRUE)

# Calculating the outliers for the TotalExp variable 

ShortECMENoutliers_summary_stats <- ShortECMEN %>%
  summarise(
    Q1 = quantile(TotalExp, 0.25, na.rm = TRUE),
    Q3 = quantile(TotalExp, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR
  )


ECMENRiceProducers <- PSAMSRiceIncome %>% 
  left_join(RiceProductionData, by = c("interview_key", "RiceType", "RiceProduced")) %>%
  # filter ECMEN outliers
  left_join(ShortECMEN, by = "interview_key") %>% 
  filter(TotalExp >= ShortECMENoutliers_summary_stats$LowerBound[1] & TotalExp <= ShortECMENoutliers_summary_stats$UpperBound[1]) %>%
  # Mutate character variables to factor variables
  mutate_if(is.character, as.factor) %>%
  mutate(TotalRiceIncomeUSD = TotalRiceIncome / 4100,
         TotalFoodExpUSD = TotalFoodExp / 4100,
         TotalExp = TotalExp/4100) %>% 
  filter(!is.na(RiceProduced)) %>% 
  filter(!is.na(HHHEthnicity)) %>%
  filter(RiceProduced != "Both") %>%
  # Calculate the total rice income per capita and household food expenditure per capita
  mutate(TotalRiceIncomePerCapitaUSD = TotalRiceIncomeUSD / HHList,
         TotalFoodExpPerCapitaUSD = TotalFoodExpUSD / HHList) %>% 
  # Calculate total food expenditure per capita per year
  mutate(TotalFoodExpPerCapitaUSDYr = TotalFoodExpPerCapitaUSD * 12) %>% 
  # Calculate rice income per month
  mutate(TotalRiceIncomePerCapitaUSDM = TotalRiceIncomePerCapitaUSD / 12) %>% 
  # Calculate food expenditure share
  mutate(FoodExpShare = round((TotalFoodExp / TotalExp) * 100, 2))

# Basic / Naive Regression Model
model <- lm(TotalExpUSD ~ TotalRiceIncomeUSD + HHHEthnicity + HHHSex + RiceProduced, data = ECMENRiceProducers)

# Interaction Term Model (More meaningful model)

model_interaction <- lm(TotalExpPerCapitaUSD ~  TotalRiceIncomePerCapitaUSDM*RiceProduced + HHHEthnicity + HHHSex, data = ECMENRiceProducers)

summary(model)

summary(model_interaction)


####################################################################################################################################################














