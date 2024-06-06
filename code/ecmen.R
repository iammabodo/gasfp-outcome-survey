# Load Necessary Packages -----------------------------------------------------------------

library(tidyverse)
library(labelled)
library(expss)
library(janitor)
library(gt)
library(readxl)

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
MEB <- 375158 # This is the new MEB for Cambodia. We might need to use the previous MEB for comparison.
SMEB <- 180648 # This is the new SMEB for Cambodia.We might need to use the previous SMEB for comparison.


# Loading data and calculating ECMEN --------------------------------------------
ECMENdata <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(ADMIN4Name, ACName, HHID, HHList, HHBaseline, 
         starts_with("HHExp"), HHHEthnicity, HHHLanguage,
         IDPoor, HHHSex) %>% 
  # Assign labels to grouping variables categories
  mutate(HHBaseline = case_when(
        HHBaseline == 0 ~ "Baseline",
        HHBaseline != 0 ~ "Not Baseline",
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
        HHHSex == 0 ~ "Female",
        HHHSex == 1 ~ "Male")) %>%
  # Remove rows with missing values
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
ECMENdata %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  # group_by(HHHEthnicity, HHHLanguage) %>%
  count(ECMEN) %>% 
  mutate(Percentage = 100 * n / sum(n))

  
# Compute the percentage of households that are able to meet survival needs 
ECMENdata %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity, HHHLanguage) %>%
  count(SurvivalECMEN) %>% 
  mutate(Percentage = 100 * n / sum(n))

# Compute the average economic capacity of households

ECMENdata %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity, IDPoor) %>%
  summarise(AvgEconomicCapacity = mean(TotalExpPerCapita, na.rm = TRUE),
            n = n())


# Insert variable labels for all variables 
var_label(ECMENdata$HHBaseline) <- "Was the household selected for the baseline survey?"
var_label(ECMENdata$HHHEthnicity) <- "Household Ethnicity 1 = Khmer, 2 = Non Khmer"
var_label(ECMENdata$HHHLanguage) <- "Household Language 1 = Khmer, 2 = Bunong, 3 = Other"
var_label(ECMENdata$IDPoor) <- "Does the household have a valid IDPoor card?"
var_label(ECMENdata$HHHSex) <- "Gender of the household head"
var_label(ECMENdata$TotalFoodExp) <- "Total Food Expenditure"
var_label(ECMENdata$TotalNonFoodExp) <- "Total Non Food Expenditure"
var_label(ECMENdata$TotalNonFoodIntExp) <- "Total Non Food Intermediate Expenditure"
var_label(ECMENdata$TotalExp) <- "Total Monthly Expenditure"
var_label(ECMENdata$TotalExpPerCapita) <- "Total Expenditure per capita"
var_label(ECMENdata$ECMEN) <- "Economic Capacity of the household"
var_label(ECMENdata$SurvivalECMEN) <- "Survival Capacity of the household"
var_label(ECMENdata$AvgEconomicCapacity) <- "Average Economic Capacity of the household or percapita economic capacity"
var_label(ECMENdata$ADMIN4Name) <- "Commune Name"
var_label(ECMENdata$ACName) <- "Name of the Agricultural Cooperative"



