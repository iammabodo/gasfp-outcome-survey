library(tidyverse)

## Climate Resilience Capacity  Score (CRCS)
# Load the data set

CRCSData <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>%  #Input file path here
  #Select the necessary variables for this analysis - dis aggregation modules and $PSAMSRiceIncome variable
  select(HHID, HHHSex, HHHEthnicity, HHHLanguage, starts_with("HHCRCS"), -c(HHCRCSShocks, HHCRCSFloods, HHCRCSWildFire,
                                                                             HHCRCSHeatWave, HHCRCSStorms, HHCRCSDroughts)) %>%
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
    CRCSScore >= 66 ~ "High"))

CRCSData %>% 
  # # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  # group_by(HHHEthnicity, HHHLanguage, HHHSex) %>% 
  # # Calculate the necessary indicators
  summarise(AverageCRCSScore = mean(CRCSScore, na.rm = T),
            MedianCRCSScore = median(CRCSScore, na.rm = T),
            TotalCRCSScore = sum(CRCSScore, na.rm = T)) %>% 
  # Ungroup the variables - to enable further analysis
  ungroup()

# Calculate the percent of household in different CRCSCategory
CRCSData %>% 
  group_by(CRCSCategory, HHHEthnicity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)
# Graph the results in a stacked bar chart


# Visualize the CRCS

