
library(tidyverse)
library(labelled)
library(expss)
library(janitor)
library(gt)
library(readxl)
library(extrafont)
library(ggtext)
library(openxlsx)

# Source the functions.R file
#source("~/Work 2023 -Eltone/General M&E/GASFP Project/gasfp-outcome-survey/code/functions.R")

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
NewMEB <- 375158 # This is the new MEB for Cambodia. 
NewSMEB <- 180648 # This is the new SMEB for Cambodia.
OldMEB <- 323614 # This is the old MEB for Cambodia. 
OldSMEB <- 159181 # This is the old SMEB for Cambodia.
OldMEBUSD <- round((OldMEB / 4100),2) # This is the old MEB in USD
OldSMEBUSD <- round((OldSMEB / 4100),2) # This is the old SMEB in USD
  
# Loading data and calculating ECMEN --------------------------------------------
ECMENdata <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage, 
         starts_with("HHExp")) %>%
  # Rename some of the variables we will be using
  rename(HHExpFAnimMeat_GiftAid_7D = HHExpFAnimMeat_GiftAid,
         HHExpFAnimMeat_Own_MN_7D = HHExpFAnimMeat_Own_MN,
         HHExpFAnimFish_GiftAid_MN_7D =  HHExpFAnimFish_GiftAid_MN,
         HHExpNFAlcTobac_Purch_MN_1M = HHExpNFAlcTobac_Purch_MN_1,
         HHExpNFAlcTobac_GiftAid_MN_1M = HHExpNFAlcTobac_GiftAid_MN,
         HHExpNFMedServ_GiftAid_MN_6M = HHExpNFMedServ_GiftAid_MN_6,
         HHExpNFMedGood_GiftAid_MN_6M = HHExpNFMedGood_GiftAid_MN_6,
         HHExpNFEduGood_GiftAid_MN_6M = HHExpNFEduGood_GiftAid_MN,
         HHExpNFHHSoft_GiftAid_MN_6M = HHExpNFHHSoft_GiftAid_MN_6,
         HHExpNFHHMaint_GiftAid_MN_6M = HHExpNFHHMaint_GiftAid_MN) %>%
  # mutate Total Food Expenditure, Total Non Food Expenditure and Total Non Food Intermediate Expenditure by summing across relevant variables
  mutate(TotalFoodExp = rowSums(across(ends_with("_7D"))),
         TotalNonFoodExp = rowSums(across(ends_with("_1M"))),
         TotalNonFoodIntExp = rowSums(across(ends_with("_6M")))) %>% 
  # Convert TotalFoodExp to monthly by dividing by 7 and multiplying by 30
  mutate(TotalFoodExp = TotalFoodExp * 30/7) %>%
  # Convert TotalNonFoodIntExp to monthly by dividing by 6
  mutate(TotalNonFoodIntExp = TotalNonFoodIntExp / 6) %>%
  # Convert add the total food and non food expenditure. Perform the calculation row wise
  mutate(TotalExp = rowSums(across(c(TotalFoodExp, TotalNonFoodIntExp, TotalNonFoodExp)))) %>% 
  # Mutate FoodExpenditure Share by dividing the TotalFoodExp by the TotalExp
  mutate(FoodExpenditureShare = round((TotalFoodExp / TotalExp)*100,2)) %>%
  # Convert the total expenditure to per capita values (Economic Capacity) i.e., by dividing by the number of household members
  mutate(TotalExpPerCapita = TotalExp / HHList) %>%
  # Convert the TotalExpPerCapita to USD
  mutate(TotalExpPerCapitaUSD = TotalExpPerCapita / 4100) %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  # Create the ECMEN variable by comparing the TotalExpPerCapita by the Minimum Expenditure Basket (MEB)
  mutate(ECMEN = case_when(
    TotalExpPerCapita >=  OldMEB ~ "Able to meet essential needs",
    
    TotalExpPerCapita < OldMEB ~ "Unable to meet essential needs"
  )) %>%
  # Calculate survival ecmen
  mutate(SurvivalECMEN = case_when(
    TotalExpPerCapita >=  OldSMEB ~ "Able Survive",
    TotalExpPerCapita < OldSMEB ~ "Unable to Survive"
  )) %>% 
  filter(TotalExpPerCapitaUSD < 500) %>% 
  # Change character variables to factors
  mutate(across(where(is.character), as.factor)) %>% 
  # Insert Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    ADMIN4Name = "Commune Name",
    ACName = "Agriculture Cooperative Name",
    HHID = "Household ID",
    HHList = "Family Size",
    HHBaseline = "Baseline vs New Member",
    IDPoor = "ID Poor Status",
    HHHSex = "Household Head Sex",
    RespSex = "Respondent Sex",
    HHHEthnicity = "Household Head Ethnicity",
    HHHLanguage = "Household Head Language",
    TotalFoodExp = "Total Household Food Expenditure",
    TotalNonFoodExp = "Total Household Non Food Expenditure",
    TotalNonFoodIntExp = "Total Household Non Food Intermediate Expenditure",
    TotalExp = "Total Household Expenditure",
    TotalExpPerCapita = "Total Household Expenditure Per Capita",
    TotalExpPerCapitaUSD = "Total Household Expenditure Per Capita in USD",
    FoodExpenditureShare = "Food Expenditure Share",
    ECMEN = "Economic Capacity To Meet Essential Needs Status",
    SurvivalECMEN = "Survival Economic Capacity To Meet Essential Needs Status"
  )


RiceProducedData <- read_excel("data/RiceProduced.xlsx") 

# Join the two datasets
ECMENdata <- left_join(ECMENdata, RiceProducedData, by = "interview_key")

ECMENdata <- ECMENdata %>% 
  mutate(RiceProduced = case_when(
    is.na(RiceProduced) ~ "No Rice Production",
    TRUE ~ RiceProduced),
    RiceType = case_when(
      is.na(RiceType) ~ "No Rice Production",
      TRUE ~ RiceType)) %>% 
  # Mutate the rice produced and rice type to a factor
  mutate(RiceProduced = as.factor(RiceProduced),
         RiceType = as.factor(RiceType))

############################################################END OF DATA CLEANING##############################################################################

# Indicator 1: Overall Percentage of households that are unable to meet essential needs

OveralECMEN <- ECMENdata %>% 
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Mutate a variable and assign a value to it which is overal ecmen
  mutate(Disagregation = "Overall") %>% 
  rename(ECMENStatus = "ECMEN") %>% 
  select(ECMENStatus, Disagregation, everything())
  

# Indicator 2: Percentage of households that are unable to meet essential needs, dis aggregated gender of the household heard

HHHSexECMEN <- ECMENdata %>% 
  group_by(HHHSex) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>%
  rename(Disagregation = HHHSex) %>%
  rename(ECMENStatus = "ECMEN") %>% 
  select(ECMENStatus, Disagregation, everything())
  
  # Indicator 3: Percentage of households that are unable to meet essential needs, dis aggregated by ethnicity
  
  HHHEthnicityECMEN <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = HHHEthnicity) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
    filter(Disagregation != "Foreigners")
  
  
  # Indicator 4: Percentage of households that are unable to meet essential needs, dis aggregated by baseline status
 ECMENBaseline <- ECMENdata %>% 
    group_by(HHBaseline) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = HHBaseline) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
   # Filter to include only people from the baseline and new members
    filter(Disagregation != "Don't Know") 
 
 # Economic Capacity by IDPoor
 ECMENIDPoor <- ECMENdata %>% 
    group_by(IDPoor) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = IDPoor) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
    filter(Disagregation != "Don't Know")
 
 # Economic Capacity by RiceProduced
 ECMENRiceProduced <- ECMENdata %>% 
    group_by(RiceProduced) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = RiceProduced) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) 
 

  # Combine the three tables into one
  ECMENIndicators <- bind_rows(OveralECMEN, HHHSexECMEN, HHHEthnicityECMEN, ECMENBaseline, ECMENIDPoor, ECMENRiceProduced) %>% 
  # Change all character variables to factors
  mutate_if(is.character, as.factor)
  
  # Write this into an xlsx file
  write.xlsx(ECMENIndicators, "report/ECMENIndicators.xlsx")
  
  write.xlsx(ECMENRiceProduced, "report/ECMENRiceProduced.xlsx")

##Calculate the economic capacity of the households(Avergae per capita expenditure)
  
# Survival MEB
  SurvivalECMENTot <- ECMENdata %>% 
    count(SurvivalECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
    # Mutate a variable and assign a value to it which is overal ecmen
    mutate(Disagregation = "Overall") %>% 
    rename(ECMENStatus = "SurvivalECMEN") %>% 
    select(ECMENStatus, Disagregation, everything())
  
  # Survival ECMEN disagregated by Gender of the household head
  SurvivalECMENHHHSex <- ECMENdata %>% 
    group_by(HHHSex) %>%
    count(SurvivalECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2)) %>%
    rename(Disagregation = HHHSex) %>%
    rename(ECMENStatus = "SurvivalECMEN") %>% 
    select(ECMENStatus, Disagregation, everything())
  
  # Survival ECMEN Disaggregated by HHHEthnicity
  SurvivalECMENHHHEthnicity <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    count(SurvivalECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = HHHEthnicity) %>%
    rename(ECMENStatus = "SurvivalECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
    filter(Disagregation != "Foreigners")
  
  # Survival ECMEN Disaggregated by Rice Produced
  SurvivalECMENRiceProduced <- ECMENdata %>% 
    group_by(RiceProduced) %>%
    count(SurvivalECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = RiceProduced) %>%
    rename(ECMENStatus = "SurvivalECMEN") %>% 
    select(ECMENStatus, Disagregation, everything())
  
  
  SurviaECMENIndicators <- bind_rows(SurvivalECMENTot, SurvivalECMENHHHSex, SurvivalECMENHHHEthnicity, SurvivalECMENRiceProduced) %>% 
    # Change all character variables to factors
    mutate_if(is.character, as.factor)
  
  # Write this into an xlsx file
  write.xlsx(SurviaECMENIndicators, "report/SurviaECMENIndicators.xlsx")
  
  # Indicator 4: Average economic per capita capacity 
  ECMENIncTot <- ECMENdata %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    mutate(Disagregation = "Overall")
  
  # Indicator 5: Average economic per capita capacity, dis aggregated by gender of the household head
  
  ECMENIncHHHSex <- ECMENdata %>%
    group_by(HHHSex) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = HHHSex)
  
  # Indicator 6: Average economic per capita capacity, dis aggregated by Ethnicity of the household head
  ECMENIncHHHEthnicity <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = HHHEthnicity)
  
  # Indicator disagregrated by idpoor
  ECMENIncIDPoor <- ECMENdata %>%
    group_by(IDPoor) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = IDPoor)
  
  # Indicator disagregrated by rice produced
  ECMENIncRiceProduced <- ECMENdata %>%
    group_by(RiceProduced) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = RiceProduced)

# Combine the three tables into one
ECMENIncIndicators <- bind_rows(ECMENIncTot, ECMENIncHHHSex, ECMENIncHHHEthnicity, ECMENIncIDPoor, ECMENIncRiceProduced) %>% select(Disagregation, AvgEconomicCapacityUSD)

# Write this into an xlsx file
write.xlsx(ECMENIncIndicators, "report/ECMENIncIndicators.xlsx")

write.xlsx(ECMENIncRiceProduced, "report/ECMENIncRiceProduced.xlsx")

############################################################END OF INDICATOR CALCULATION########################################  












