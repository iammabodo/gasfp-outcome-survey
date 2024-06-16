
library(tidyverse)
library(labelled)
library(expss)
library(janitor)
library(gt)
library(readxl)


# Source the functions.R file
source("~/Work 2023 -Eltone/General M&E/GASFP Project/gasfp-outcome-survey/code/functions.R")

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
MEB <- 375158 # This is the new MEB for Cambodia. We might need to use the previous MEB for comparison.
SMEB <- 180648 # This is the new SMEB for Cambodia.We might need to use the previous SMEB for comparison.


# Loading data and calculating ECMEN --------------------------------------------
ECMENdata <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(ADMIN4Name, ACName, HHID, HHList, HHBaseline, 
         starts_with("HHExp"), HHHEthnicity, HHHLanguage,
         IDPoor, SEX_HHH) %>% 
  # Assign labels to grouping variables categories
  mutate(HHBaseline = case_when(
        HHBaseline == 1 ~ "Baseline Members",
        HHBaseline == 0 ~ "New Members",
        TRUE ~ "Don't Know",),
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
        HHHSex = case_when(
        SEX_HHH == 0 ~ "Female",
        SEX_HHH == 1 ~ "Male"),
        ADMIN4Name = case_when(
        ADMIN4Name == 100 ~ "Nang Khi Loek",
        ADMIN4Name == 200 ~ "Ou Buon Leu",
        ADMIN4Name == 300 ~ "Roya",
        ADMIN4Name == 400 ~ "Sokh Sant",
        ADMIN4Name == 500 ~ "Sre Huy",
        ADMIN4Name == 600 ~ "Sre Sangkom",
        TRUE ~ "Other"),
        ACName = case_when(
        ACName == 1 ~ "Phum Srae Huy",
        ACName == 2 ~ "Samak Mean Rith Rung Roeung",
        ACName == 3 ~ "Samaki Phum Toul",
        ACName == 4 ~ "Apiwat Mean Chey",
        ACName == 5 ~ "Samaki Rik Chom Roeung")) %>%
  # Remove outliers
  # find_outliers() %>% Uncomment this line to remove outliers
  # mutate a variable by summing across variables that contains _7
  mutate(TotalFoodExp = rowSums(across(contains("_7"))),
         TotalNonFoodExp = rowSums(across(contains("_1M"))),
         TotalNonFoodIntExp = rowSums(across(contains("_6M")))) %>% 
  # Convert TotalFoodExp to monthly by dividing by 7 and multiplying by 30
  mutate(TotalFoodExp = TotalFoodExp * 30/7) %>%
  # Convert TotalNonFoodIntExp to monthly by dividing by 6
  mutate(TotalNonFoodIntExp = TotalNonFoodIntExp / 6) %>%
  # Convert add the total food and non food expenditure. Perform the calculation row wise
  mutate(TotalExp = rowSums(across(c(TotalFoodExp, TotalNonFoodIntExp, TotalNonFoodExp)))) %>% 
  # Convert the total expenditure to per capita values (Economic Capacity) i.e., by dividing by the number of household members
  mutate(TotalExpPerCapita = TotalExp / HHList) %>%
  # Convert the TotalExpPerCapita to USD
  mutate(TotalExpPerCapitaUSD = TotalExpPerCapita / 4100) %>%
  # Create the ECMEN variable by comparing the TotalExpPerCapita by the Minimum Expenditure Basket (MEB)
  mutate(ECMEN = case_when(
    TotalExpPerCapita >=  MEB ~ "Able to meet essential needs",
    TotalExpPerCapita < MEB ~ "Unable to meet essential needs"
  )) %>%
  # Calculate survival ecmen
  mutate(SurvivalECMEN = case_when(
    TotalExpPerCapita >=  SMEB ~ "Able Survive",
    TotalExpPerCapita < SMEB ~ "Unable to Survive"
  ))


# Compute the percentage of households that are able to meet essential needs
OveralECMEN <- ECMENdata %>% 
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2))

IDPoorECMEN <- ECMENdata %>% 
  group_by(IDPoor) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Filter out the households that are not able to meet essential needs
  filter(ECMEN == "Unable to meet essential needs")

HHHSexECMEN <- ECMENdata %>% 
  group_by(HHHSex) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Filter out the households that are not able to meet essential needs
  filter(ECMEN == "Unable to meet essential needs")

HHHEthnicityECMEN <- ECMENdata %>%
  group_by(HHHEthnicity) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Filter out the households that are not able to meet essential needs
  filter(ECMEN == "Unable to meet essential needs")
  

HHHLanguageECMEN <- ECMENdata %>%
  group_by(HHHLanguage) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Filter out the households that are not able to meet essential needs
  filter(ECMEN == "Unable to meet essential needs")

# Compute the average economic capacity of households
ECMENIncTot <- ECMENdata %>%
  summarise(AvgEconomicCapacityUSD = mean(TotalExpPerCapitaUSD, na.rm = TRUE),
            n = n())

ECMENIncIDPoor <- ECMENdata %>%
  group_by(IDPoor) %>%
  summarise(AvgEconomicCapacityUSD = mean(TotalExpPerCapitaUSD, na.rm = TRUE),
            n = n())
ECMENIncHHHSex <- ECMENdata %>%
  group_by(HHHSex) %>%
  summarise(AvgEconomicCapacityUSD = mean(TotalExpPerCapitaUSD, na.rm = TRUE),
            n = n())

ECMENIncHHHEthnicity <- ECMENdata %>%
  group_by(HHHEthnicity) %>%
  summarise(AvgEconomicCapacityUSD = mean(TotalExpPerCapitaUSD, na.rm = TRUE),
            n = n())