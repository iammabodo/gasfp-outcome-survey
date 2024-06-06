library(tidyverse)
library(labelled)
library(expss)
library(readxl)


#Import the data set
LCSENdata <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>% 
  #Select necessary variables, in this case the livelihoods coping strategies and disaggretion variables
  select(HHID, HHHSex, HHHEthnicity, HHHLanguage, starts_with("LcsEN"), -(contains("LcsENAccess__"))) %>%
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
    TRUE ~ "NA"))





#create a variable to specify if the household used any of the strategies by severity
#stress

var_label(data$stress_coping_EN) <- "Did the HH engage in stress coping strategies"
#Crisis

var_label(data$crisis_coping_EN) <- "Did the HH engage in crisis coping strategies"
#Emergency

var_label(data$emergency_coping_EN) <- "Did the HH engage in emergency coping strategies"

#calculate Max_coping_behaviour

var_label(data$Max_coping_behaviourEN) <- "Summary of asset depletion"
val_lab(data$Max_coping_behaviourEN) = num_lab("
             1 HH not adopting coping strategies
             2 Stress coping strategies
             3 Crisis coping strategies
             4 Emergencies coping strategies")


#creates a table of the weighted percentage of Max_coping_behaviourFS by
#creating a temporary variable to display value labels 
#and providing the option to use weights if needed


Max_coping_behaviourEN_table_wide <- LCSENdata %>% 
  drop_na(Max_coping_behaviourEN) %>%
  # Group by HHHSex HHHSex, HHHEthnicity, HHHLanguage, 
  group_by(HHHEthnicity, HHHLanguage) %>%
  count(Max_coping_behaviourEN) %>% # if weights are needed use instead the row below 
  mutate(Percentage = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = Max_coping_behaviourEN,
              values_from = Percentage,
              values_fill =  0) 


