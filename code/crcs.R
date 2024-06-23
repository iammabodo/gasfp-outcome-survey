library(tidyverse)

## Climate Resilience Capacity  Score (CRCS)

#######################################LOADING DATA####################################################

CRCSData <- read_excel("data/FullHHRosterClean.xlsx") %>%  #Input file path here
  #Select the necessary variables for this analysis - dis aggregation modules and $PSAMSRiceIncome variable
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage,
         starts_with("HHCRCS")) %>%
  # Create HHHEthnicity, SEX_HHH and HHHLanguage variables to be more descriptive 
  # Create the CRCSScore variable by summing across the variables that contains HHCRCS
  mutate(CRCSScore = rowSums(across(contains("HHCRCS")))) %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  # Normalize the CRCSScore by dividing by 9
  mutate(CRCSScore = (CRCSScore / 9) - 1) %>%
  # Divide the CRCSScore by 4, then multiply by 100 to get the percentage
  mutate(CRCSScore = (CRCSScore / 4) * 100) %>%
  # Create the CRCSCategory variable
  mutate(CRCSCategory = case_when(
    CRCSScore < 33 ~ "Low",
    CRCSScore >= 33 & CRCSScore < 66 ~ "Medium",
    CRCSScore >= 66 ~ "High")) %>% 
  # Calculate Anticipatory Capacity
  mutate(AnticipatoryCapacity = (HHCRCSPrepared - 1)/4) %>% 
  mutate(AnticipatoryCapacity = AnticipatoryCapacity * 100) %>%
  # Calculate Absorptive Capacity
  mutate(AbsorptiveCapacity = (HHCRCSBounceBack - 1)/4) %>%
  mutate(AbsorptiveCapacity = AbsorptiveCapacity * 100) %>% 
  # Calculate Transformative Capacity
  mutate(TransformativeCapacity = (HHCRCSIncSrcChange - 1)/4) %>%
  mutate(TransformativeCapacity = TransformativeCapacity * 100) %>%
  # Calcualate Adaptive Capacity
  mutate(AdaptiveCapacity = (HHCRCSGetBy - 1)/4) %>%
  mutate(AdaptiveCapacity = AdaptiveCapacity * 100) %>% 
  # Calculate the categorised capacities for AnticipatoryCapacity, AbsorptiveCapacity, TransformativeCapacity and AdaptiveCapacity
  mutate(AnticipatoryCapacityCategory = case_when(
    AnticipatoryCapacity < 33 ~ "Low",
    AnticipatoryCapacity >= 33 & AnticipatoryCapacity < 66 ~ "Medium",
    AnticipatoryCapacity >= 66 ~ "High")) %>%
  mutate(AbsorptiveCapacityCategory = case_when(
    AbsorptiveCapacity < 33 ~ "Low",
    AbsorptiveCapacity >= 33 & AbsorptiveCapacity < 66 ~ "Medium",
    AbsorptiveCapacity >= 66 ~ "High")) %>%
  mutate(TransformativeCapacityCategory = case_when(
    TransformativeCapacity < 33 ~ "Low",
    TransformativeCapacity >= 33 & TransformativeCapacity < 66 ~ "Medium",
    TransformativeCapacity >= 66 ~ "High")) %>%
  mutate(AdaptiveCapacityCategory = case_when(
    AdaptiveCapacity < 33 ~ "Low",
    AdaptiveCapacity >= 33 & AdaptiveCapacity < 66 ~ "Medium",
    AdaptiveCapacity >= 66 ~ "High"))


###################################################INDICATOR CALCULATION####################################################

CRCSData %>% 
  group_by(AnticipatoryCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)

CRCSData %>% 
  group_by(AbsorptiveCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)


CRCSData %>%
  group_by(TransformativeCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)

CRCSData %>% 
  group_by(AdaptiveCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)



CRCSData %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity) %>% 
  # Calculate the necessary indicators
  summarise(AverageCRCSScore = mean(CRCSScore, na.rm = T)) %>% 
  # Ungroup the variables - to enable further analysis
  ungroup()
   
# Calculate the percent of household in different CRCSCategory
CRCSData %>% 
  group_by(CRCSCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)
# Graph the results in a stacked bar chart


#############################################INDICATOR VISUALISATION####################################################

