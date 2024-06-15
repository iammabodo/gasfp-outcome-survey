library(tidyverse)

## Climate Resilience Capacity  Score (CRCS)
# Load the data set


CRCSData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx",
                       sheet = 1) %>%  #Input file path here
  #Select the necessary variables for this analysis - dis aggregation modules and $PSAMSRiceIncome variable
  select(HHID, SEX_HHH, HHHEthnicity, HHHLanguage, starts_with("HHCRCS"), -c(HHCRCSShocks, HHCRCSFloods, HHCRCSWildFire,
                                                                             HHCRCSHeatWave, HHCRCSStorms, HHCRCSDroughts)) %>%
  # Create HHHEthnicity and HHHLanguage variables to be more descriptive
  mutate(HHHEthnicity = case_when(
    HHHEthnicity == 3 | HHHEthnicity == 11 | HHHEthnicity == 12 ~ "Ethnic Minority",
    HHHEthnicity == 999 ~ "Other",
    HHHEthnicity == 888 ~ "Don't Know / prefer not to answer",
    TRUE ~ "Indigenous"),
    HHHLanguage = case_when(
    HHHLanguage == 1 ~ "Khmer",
    HHHLanguage == 2 ~ "Bunong",
    TRUE ~ "Other")) %>%
  # Create the CRCSScore variable by summing across the variables that contains HHCRCS
  mutate(CRCSScore = rowSums(across(contains("HHCRCS")))) %>%
  # Normalize the CRCSScore by dividing by 9
  mutate(CRCSScore = (CRCSScore / 9) - 1) %>%
  # Divide the CRCSScore by 4, then multiply by 100 to get the percentage
  mutate(CRCSScore = (CRCSScore / 4) * 100) %>%
  # Create the CRCSCategory variable
  mutate(CRCSCategory = case_when(
    CRCSScore < 33 ~ "Low",
    CRCSScore >= 33 & CRCSScore < 66 ~ "Medium",
    CRCSScore >= 66 ~ "High")) #%>% 
  #find_outliers() # Remove outliers by uncommenting this line


CRCSData %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity, HHHLanguage) %>% 
  # Calculate the necessary indicators
  summarise(AverageCRCSScore = mean(CRCSScore, na.rm = T)) %>% 
  # Ungroup the variables - to enable further analysis
  ungroup()

# Calculate the percent of household in different CRCSCategory
CRCSData %>% 
  group_by(CRCSCategory, HHHEthnicity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)
# Graph the results in a stacked bar chart


# Visualize the CRCS

