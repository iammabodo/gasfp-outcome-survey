library(tidyverse)
library(expss)
library(labelled)

# Import data for the calculation of all cross cutting indicators

CrossCuttingData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  select(interview_key, HHID, SEX_HHH, HHHEthnicity, HHList, HHHLanguage, IDPoor, HHBaseline, # Disagregation variables
         starts_with("HHAsst"), starts_with("HHDTP"), starts_with("RGenEntity"))
  

# Calculate CC-1.1: Beneficiaries reporting safety concerns 
SafetyConcerns <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(HHID, HHAsstSecurity, IDPoor, SEX_HHH) %>% 
  # Mutate the key variable (HHAsstSecurity) to have meaningful labels
  mutate(HHAsstSecurity = case_when(
    HHAsstSecurity == 0 ~ "No",
    HHAsstSecurity == 1 ~ "Yes"))

# Calculate the percentage of beneficiaries reporting safety concerns
SafetyConcerns %>% 
  group_by(HHAsstSecurity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  select(-Count) %>% 
  pivot_wider(names_from = HHAsstSecurity, values_from = Percentage) %>% 
  mutate(Indicator = "Beneficiaries reporting safety concerns") %>% 
  select(Indicator, everything())


#CC 1.2 Barriers to training

BarriersToTraining <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(HHID, HHAsstAccess, HHAsstAccessAction, IDPoor, SEX_HHH) %>% # Might need to add HHAsstAccessWhat here when we have the final data set
  # Mutate the key variable (HHAsstAccess) to have meaningful labels
  mutate(HHAsstAccess = case_when(
    HHAsstAccess == 0 ~ "No",
    HHAsstAccess == 1 ~ "Yes",
    TRUE ~ "Don't know"))

# Indicator Calculation
BarriersToTraining %>% 
  group_by(HHAsstAccess) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  select(-Count) %>%
  pivot_wider(names_from = HHAsstAccess, values_from = Percentage) %>%
  mutate(Indicator = "Barriers to Training") %>%
  select(Indicator, everything())

#CC 1.3 Treatment with respect and dignity

TreatedRespectfully <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(HHID, HHAsstRespect, HHDTPDign, IDPoor, SEX_HHH) %>% 
  # Mutate the key variables (HHAsstRespect and HHDTPDign) to have meaningful labels
  mutate(HHAsstRespect = case_when(
    HHAsstRespect == 0 ~ "No",
    HHAsstRespect == 1 ~ "Yes"),
    HHDTPDign = case_when(
    HHDTPDign == 0 ~ "No",
    HHDTPDign == 1 ~ "Yes")) %>% 
  # Mutate the key variable (Treated with respect and dignity at the program site) to have meaningful labels
  mutate(HHAsstRespectDign = case_when(
    HHAsstRespect == "Yes" & HHDTPDign == "Yes" ~ "Yes",
    TRUE ~ "No"))

# Indicator Calculation
TreatedRespectfully %>% 
  group_by(HHAsstRespectDign) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  select(-Count) %>%
  pivot_wider(names_from = HHAsstRespectDign, values_from = Percentage) %>%
  mutate(Indicator = "Treated with respect and dignity") %>%
  select(Indicator, everything())


# 2.1 Accessible Information
AccessibleInformation <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(HHID, IDPoor, SEX_HHH, # Disagregation variables
         HHAsstKnowEnt, HHAsstKnowPeople, HHAsstRecInfo, HHAsstReportMisc) %>% # Indicator calculation variables
  # Mutate the key variables to have meaningful labels
  mutate(HHAsstKnowEnt = case_when(
    HHAsstKnowEnt == 0 ~ "No",
    HHAsstKnowEnt == 1 ~ "Yes"),
    HHAsstKnowPeople = case_when(
    HHAsstKnowPeople == 0 ~ "No",
    HHAsstKnowPeople == 1 ~ "Yes"),
    HHAsstRecInfo = case_when(
    HHAsstRecInfo == 0 ~ "No",
    HHAsstRecInfo == 1 ~ "Yes",
    HHAsstRecInfo == 3 ~ "I never received information"),
    HHAsstReportMisc = case_when(
    HHAsstReportMisc == 0 ~ "No",
    HHAsstReportMisc == 1 ~ "Yes")) %>% 
  # Create the Accessible Information variable
  mutate(AccessibleInformation = case_when(
    HHAsstKnowEnt == "Yes" & HHAsstKnowPeople == "Yes" & HHAsstRecInfo == "Yes" & HHAsstReportMisc == "Yes" ~ "Yes",
    TRUE ~ "No"))


# Indicator Calculation
AccessibleInformation %>% 
  group_by(AccessibleInformation) %>%  # To include other disaggregation variables here once we have the final data
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  select(-Count) %>%
  pivot_wider(names_from = AccessibleInformation, values_from = Percentage) %>%
  mutate(Indicator = "Accessible Information") %>%
  select(Indicator, everything())


# 3.4 Community Meaningful Participation

# Import the data with additional variables (gender and age of the respondent)

GenderData <- read_excel("data/WFP_GASFP_WO8_NumericV2.xlsx") %>% 
  select(interview_key, HHID, Part19_HHMemberID, Part19_RespGender, Part19_RespAge) %>% 
  # Rename the variables
  rename(HHMemberID = Part19_HHMemberID,
         RespGender = Part19_RespGender,
         RespAge = Part19_RespAge) %>% 
  # Change RespGender to a more meaningful variable
  mutate(RespGender = case_when(
    RespGender == 1 ~ "Male",
    RespGender == 0 ~ "Female"))

CommunityParticipation <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHID, IDPoor, # Disagregation variables
         starts_with("RGenEntity")) %>% # Indicator calculation variables
  # Join with Gender data
  left_join(GenderData, by = "interview_key") %>%
  # Mutate the key variables to have meaningful labels
  mutate(RGenEntityYN = case_when(
    RGenEntityYN == 0 ~ "No",
    RGenEntityYN == 1 ~ "Yes"),
    RGenEntityRights = case_when(
    RGenEntityRights == 0 ~ "No",
    RGenEntityRights == 1 ~ "Yes"),
    RGenEntityMeet = case_when(
    RGenEntityMeet == 0 ~ "No",
    RGenEntityMeet == 1 ~ "Yes"),
    RGenEntityOp = case_when(
    RGenEntityOp == 0 ~ "No",
    RGenEntityOp == 1 ~ "Yes"),
    RGenEntityNeg = case_when(
    RGenEntityNeg == 0 ~ "No",
    RGenEntityNeg == 1 ~ "Yes"),
    RGenEntityViab = case_when(
    RGenEntityViab == 0 ~ "No",
    RGenEntityViab == 1 ~ "Yes"),
    RGenEntityDM = case_when(
    RGenEntityDM == 0 ~ "No",
    RGenEntityDM == 1 ~ "Yes")) %>%
  # Filter to have only those who answered "Yes" to the variable RGenEntityYN (i.e AC management committee)
  filter(RGenEntityYN == "Yes") %>%
  # Create the Community Participation variable
  mutate(CommunityParticipation = case_when(
    RGenEntityNeg == "Yes" | RGenEntityViab == "Yes" | RGenEntityDM == "Yes" ~ "Meaningful Participation",
    TRUE ~ "No Meaningful Participation"))

# Calculate the percentage of beneficiaries reporting meaningful participation

CommunityParticipation %>% 
  group_by(RespGender, CommunityParticipation) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(CommunityParticipation == "Meaningful Participation")








