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

AnticipatoryCapacity <- CRCSData %>% 
  group_by(AnticipatoryCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)%>% 
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = AnticipatoryCapacityCategory, values_from = Percentage) %>% 
  mutate(Disagregation = "Anticipatory Capacity") %>% 
  select(Disagregation, Low, Medium, High)

AbsorptiveCapacity <- CRCSData %>% 
  group_by(AbsorptiveCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = AbsorptiveCapacityCategory, values_from = Percentage) %>% 
  mutate(Disagregation = "Absorptive Capacity") %>% 
  select(Disagregation, Low, Medium, High)


TransformativeCapacity <- CRCSData %>%
  group_by(TransformativeCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)%>% 
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = TransformativeCapacityCategory, values_from = Percentage) %>% 
  mutate(Disagregation = "Transformative Capacity") %>% 
  select(Disagregation, Low, Medium, High)

AdaptiveCapacity <- CRCSData %>% 
  group_by(AdaptiveCapacityCategory) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100)%>% 
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = AdaptiveCapacityCategory, values_from = Percentage) %>% 
  mutate(Disagregation = "Adaptive Capacity") %>% 
  select(Disagregation, Low, Medium, High)

# Calculate the percent of household in different CRCSCategory
CRCSTotal <- CRCSData %>%
  filter(!is.na(CRCSCategory)) %>%  # Remove NAs in CRCSCategory
  group_by(CRCSCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Calculate count
  mutate(Percentage = (Count / sum(Count)) * 100) %>%  # Calculate percentage
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = CRCSCategory, values_from = Percentage) %>% 
  mutate(Disagregation = "Overall") %>% 
  select(Disagregation, Low, Medium, High)

CRCSQuantTot <- CRCSData %>%
  summarise(AvgCRCS = mean(CRCSScore, na.rm = TRUE)) %>% 
  mutate(Disagregation = "Overall")

CRCSQuantEthnicity <- CRCSData %>%
  group_by(HHHEthnicity) %>%
  summarise(AvgCRCS = mean(CRCSScore, na.rm = TRUE)) %>% 
  rename(Disagregation = HHHEthnicity) %>% 
  filter(Disagregation != "Foreigners")

CRCSQuantSex <- CRCSData %>%
  group_by(HHHSex) %>%
  summarise(AvgCRCS = mean(CRCSScore, na.rm = TRUE)) %>% 
  rename(Disagregation = HHHSex)

# Bind the tables together
CRCSQuantIndicators <- bind_rows(CRCSQuantTot, CRCSQuantEthnicity, CRCSQuantSex) %>% 
  mutate_if(is.character, as.factor) %>% 
  #round the AvgCRCS to 2 decimal places
  mutate(AvgCRCS = round(AvgCRCS, 2))

write.xlsx(CRCSQuantIndicators, "report/CRCSQuantIndicators.xlsx")


# Calculate CRCS by baseline status
CRCSCategoryBaseline <- CRCSData %>%
  filter(!is.na(CRCSCategory)) %>%  # Remove NAs in CRCSCategory
  group_by(HHBaseline, CRCSCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Calculate count
  mutate(Percentage = (Count / sum(Count)) * 100) %>%  # Calculate percentage
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = CRCSCategory, values_from = Percentage) %>% 
  mutate(Disagregation = HHBaseline) %>% 
  select(Disagregation, Low, Medium, High) %>% 
  filter(Disagregation != "Don't Know")

CRCSCategoryEthnicity <- CRCSData %>%
  filter(!is.na(CRCSCategory)) %>%  # Remove NAs in CRCSCategory
  group_by(HHHEthnicity, CRCSCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Calculate count
  mutate(Percentage = (Count / sum(Count)) * 100) %>%  # Calculate percentage
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = CRCSCategory, values_from = Percentage) %>% 
  mutate(Disagregation = HHHEthnicity) %>% 
  filter(Disagregation != "Foreigners")

CRCSCategorySex <- CRCSData %>%
  filter(!is.na(CRCSCategory)) %>%  # Remove NAs in CRCSCategory
  group_by(HHHSex, CRCSCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Calculate count
  mutate(Percentage = (Count / sum(Count)) * 100) %>%  # Calculate percentage
  select(-Count) %>%  # Remove the Count column
  pivot_wider(names_from = CRCSCategory, values_from = Percentage) %>% 
  mutate(Disagregation = HHHSex)

# Combine the tables into one

CRCSIndicators <- bind_rows(AnticipatoryCapacity, AbsorptiveCapacity, TransformativeCapacity, 
                            AdaptiveCapacity, CRCSTotal, CRCSCategoryBaseline, CRCSCategoryEthnicity, CRCSCategorySex) %>% 
  mutate_if(is.character, as.factor) %>% 
  #round the everything that is numeric to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Write an excel file 
write.xlsx(CRCSIndicators, "report/CRCSIndicators.xlsx")

#############################################INDICATOR VISUALISATION####################################################
CRCSData %>% 
  ggplot(aes(x = CRCSCategory)) +
  geom_bar() +
  theme_bw()
