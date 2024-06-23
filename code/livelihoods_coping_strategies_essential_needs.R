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

# Stress
var_label(LCSENdata$LcsEN_stress_DomAsset) <- "Engage in stress coping strategies by selling domestic assets"
var_label(LCSENdata$LcsEN_stress_Saving) <- "Engage in stress coping strategies by spending savings"
var_label(LCSENdata$LcsEN_stress_BorrowCash) <- "Engage in stress coping strategies by borrowing cash"
var_label(LCSENdata$LcsEN_stress_HHSeparation) <- "Engage in stress coping strategies by separation of family members"
var_label(LCSENdata$stress_coping_EN) <- "Did the HH engage in stress coping strategies"

#Crisis
var_label(LCSENdata$LcsEN_crisis_ProdAssets) <- "Engage in crisis coping strategies by selling productive assets"
var_label(LCSENdata$LcsEN_crisis_Health) <- "Engage in crisis coping strategies by spending on health"
var_label(LCSENdata$LcsEN_crisis_OutSchool) <- "Engage in crisis coping strategies by taking children out of school"
var_label(LCSENdata$LcsEN_crisis_Edu) <- "Engage in crisis coping strategies by reducing education expenses"
var_label(LCSENdata$crisis_coping_EN) <- "Did the HH engage in crisis coping strategies"


# Emergency
var_label(LCSENdata$LcsEN_em_ResAsset) <- "Engage in emergency coping strategies by selling residential assets e.g., house"
var_label(LCSENdata$LcsEN_em_Begged) <- "Engage in emergency coping strategies by begging"
var_label(LCSENdata$LcsEN_em_IllegalAct) <- "Engage in emergency coping strategies by engaging in illegal activities"
var_label(LCSENdata$emergency_coping_EN) <- "Did the HH engage in emergency coping strategies"

# Calculate Max_coping_behavior


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
              values_fill =  0) %>% 
  drop_na()

# Graph maximum coping behaviour
Max_coping_behaviourEN_table_wide %>%
  gather(Max_coping_behaviourEN, Percentage, -HHHEthnicity, -HHHLanguage) %>%
  ggplot(aes(x = HHHEthnicity, y = Percentage, fill = Max_coping_behaviourEN)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Maximum coping")
