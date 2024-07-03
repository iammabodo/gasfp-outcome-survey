library(tidyverse)
library(labelled)
library(expss)
library(readxl)
library(openxlsx)

  
#Import the data set
LCSENdata <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  #Select necessary variables, in this case the livelihoods coping strategies and disaggretion variables
  select(interview_key, HHID, HHList, HHBaseline, HHHEthnicity, HHHLanguage, IDPoor, HHHSex, RespSex, ADMIN4Name, 
         starts_with("LcsEN")) %>%
  #Add the necessary labels to the variables
  mutate(across(c(LcsEN_stress_DomAsset,LcsEN_stress_Saving,LcsEN_stress_BorrowCash, LcsEN_stress_HHSeparation, # Stress coping strategies
                  LcsEN_crisis_ProdAssets,LcsEN_crisis_Health,LcsEN_crisis_OutSchool, LcsEN_crisis_Edu, # Crisis coping strategies
                  LcsEN_em_ResAsset,LcsEN_em_Begged,LcsEN_em_IllegalAct # Emergency coping strategies
                  ), 
                # Adding labels to the variables
                ~labelled(., labels = c("No, because I did not need to" = 10,
                                        "No, because I already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" = 20,
                                        "Yes" = 30,
                                        "Not applicable (don’t have access to this strategy)" = 9999)))) %>% 
  # Create a variables to specify if the household used any of the strategies by severity
  mutate(
    # Stress coping strategies variable
    stress_coping_EN = case_when(
    LcsEN_stress_DomAsset == 20 |  LcsEN_stress_DomAsset == 30 ~ 1,
    LcsEN_stress_Saving == 20 | LcsEN_stress_Saving == 30 ~ 1,
    LcsEN_stress_BorrowCash == 20 | LcsEN_stress_BorrowCash == 30 ~ 1,
    LcsEN_stress_HHSeparation == 20 | LcsEN_stress_HHSeparation == 30 ~ 1,
    TRUE ~ 0),
    # Crisis coping strategies variable
    crisis_coping_EN = case_when(
    LcsEN_crisis_ProdAssets == 20 |  LcsEN_crisis_ProdAssets == 30 ~ 1,
    LcsEN_crisis_Health == 20 | LcsEN_crisis_Health == 30 ~ 1,
    LcsEN_crisis_OutSchool == 20 | LcsEN_crisis_OutSchool == 30 ~ 1,
    LcsEN_crisis_Edu == 20 | LcsEN_crisis_Edu == 30 ~ 1,
    TRUE ~ 0),
    # Emergency coping strategies variable
    emergency_coping_EN = case_when(
    LcsEN_em_ResAsset == 20 |  LcsEN_em_ResAsset == 30 ~ 1,
    LcsEN_em_Begged == 20 | LcsEN_em_Begged == 30 ~ 1,
    LcsEN_em_IllegalAct == 20 | LcsEN_em_IllegalAct == 30 ~ 1,
    TRUE ~ 0)) %>%
  # Calculate Max_coping_behavior
  mutate(Max_coping_behaviourEN = case_when(
    emergency_coping_EN == 1 ~ 4,
    crisis_coping_EN == 1 ~ 3,
    stress_coping_EN == 1 ~ 2,
    TRUE ~ 1)) %>%
  # Change the variable to a factor variable
  mutate(Max_coping_behaviourEN = case_when(
    Max_coping_behaviourEN == 1 ~ "HH not adopting coping strategies",
    Max_coping_behaviourEN == 2 ~ "Stress coping strategies",
    Max_coping_behaviourEN == 3 ~ "Crisis coping strategies",
    Max_coping_behaviourEN == 4 ~ "Emergencies coping strategies",
    TRUE ~ "NA")) %>% 
  distinct(interview_key, .keep_all = TRUE) %>% 
  filter(HHHEthnicity != "Foreigners")

# Create a table of the weighted percentage of Max_coping_behaviourEN
TotalLCSEN <- LCSENdata %>% 
  group_by(Max_coping_behaviourEN) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, everything())


# Calculate the indicator of the maximum coping behavior dis aggregated by Ethnicity
LCSENEthnicity <- LCSENdata %>% 
  group_by(HHHEthnicity, Max_coping_behaviourEN) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  rename(Disagregation = HHHEthnicity)
  
# Calculate the indicator of the maximum coping behavior dis aggregated by Gender of Household Heard
LCSENHHHSex <- LCSENdata %>% 
  group_by(HHHSex, Max_coping_behaviourEN) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  rename(Disagregation = HHHSex)

LCSENIDPoor <- LCSENdata %>% 
  group_by(IDPoor, Max_coping_behaviourEN) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  rename(Disagregation = IDPoor)


# Combine the tables
LCSENTable <- bind_rows(TotalLCSEN, LCSENEthnicity, LCSENHHHSex, LCSENIDPoor) %>% 
  # Round every numeric to 2 decimals
  mutate_if(is.numeric, ~round(., 2))

# Write an excel table
write.xlsx(LCSENTable, "report/LCSENTable.xlsx")

LCSENdata %>% 
  group_by(Max_coping_behaviourEN) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ggplot(aes(x = Max_coping_behaviourEN, y = Percentage)) +
  geom_bar(stat = "identity") +
  labs(title = "Maximum coping behaviour")


