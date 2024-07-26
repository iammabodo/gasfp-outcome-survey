library(tidyverse)
library(expss)
library(labelled)
library(gt)
library(openxlsx)

# Import data for the calculation of all cross cutting indicators

CrossCuttingData <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  select(interview_key, HHID, HHHSex, HHHEthnicity, HHList, HHHLanguage, IDPoor, AGE_Resp, RespSex, HHBaseline, # Disagregation variables
         starts_with("HHAsst"), starts_with("HHDTP"), starts_with("RGenEntity")) %>% 
  filter(AGE_Resp >= 18) %>% 
  distinct(interview_key, .keep_all = TRUE)
  
###################################################################################################################################

# Calculate CC-1.1: Beneficiaries reporting safety concerns 
SafetyConcerns <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHAsstSecurity, HHHEthnicity, RespSex) %>% 
  # Mutate the key variable (HHAsstSecurity) to have meaningful labels
  mutate(HHAsstSecurity = case_when(
    HHAsstSecurity == 0 ~ "No",
    HHAsstSecurity == 1 ~ "Yes")) %>% 
  # Set Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    HHHEthnicity = "Ethnicity of Household Head",
    RespSex = "Gender of Respondent",
    HHAsstSecurity = "Safety Concerns telated to the Program")

# Calculate the percentage of beneficiaries reporting safety concerns
SafetyConcernsTotal <- SafetyConcerns %>% 
  group_by(HHAsstSecurity) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  filter(HHAsstSecurity == "No") %>%
  select(HHAsstSecurity, Percentage) %>%
  # Change Yes to Total
  mutate(HHAsstSecurity = "Total") %>%
  rename(Disaggregation = HHAsstSecurity)

# Indicator Calculation, disaggregated by gender of the respondent
SafetyConcernsGender <- SafetyConcerns %>% 
  group_by(RespSex, HHAsstSecurity) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(HHAsstSecurity == "No") %>%
  select(RespSex, Percentage) %>%
  rename(Disaggregation = RespSex)

# Calculate the indicator, disaggregated by HHHEthnicity
SafetyConcernsEthnicity <- SafetyConcerns %>% 
  group_by(HHHEthnicity, HHAsstSecurity) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(HHAsstSecurity == "No") %>%
  select(HHHEthnicity, Percentage) %>%
  rename(Disaggregation = HHHEthnicity)

# Combine the tables for all the disaggregations
SafetyConcernsTable <- rbind(SafetyConcernsTotal, SafetyConcernsGender, SafetyConcernsEthnicity) %>% 
  #round the percentage to 2 decimal place
  mutate(Percentage = round(Percentage, 2))
  
# Write excel
write.xlsx(SafetyConcernsTable, "report/SafetyConcernsTable.xlsx")

#################################################################################################################################

#CC 1.2 Barriers to training

BarriersToTraining <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHAsstAccess, HHAsstAccessAction, HHHEthnicity, RespSex) %>% 
  # Mutate the key variable (HHAsstAccess) to have meaningful labels
  mutate(HHAsstAccess = case_when(
    HHAsstAccess == 0 ~ "No",
    HHAsstAccess == 1 ~ "Yes",
    TRUE ~ "Don't know")) %>%
  # Set Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    HHHEthnicity = "Ethnicity of Household Head",
    RespSex =  "Gender of Respondent",
    HHAsstAccess = "Barriers to Training")

# Indicator Calculation Total
BarriersToTrainingTotal <- BarriersToTraining %>% 
  group_by(HHAsstAccess) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup() %>%
  filter(HHAsstAccess == "No") %>%
  select(HHAsstAccess, Percentage) %>%
  # Change Yes to Total
  mutate(HHAsstAccess = "Total") %>%
  rename(Disaggregation = HHAsstAccess)

# Indicator Calculation, disaggregated by gender of the respondent
BarriersToTrainingGender <- BarriersToTraining %>% 
  group_by(RespSex, HHAsstAccess) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(HHAsstAccess == "No") %>%
  select(RespSex, Percentage) %>%
  rename(Disaggregation = RespSex)

# Indicator Calculation, disaggregated by HHHEthnicity

BarriersToTrainingEthnicity <- BarriersToTraining %>% 
  group_by(HHHEthnicity, HHAsstAccess) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  filter(HHAsstAccess == "No" & HHHEthnicity != "Foreigners") %>%
  select(HHHEthnicity, Percentage) %>%
  rename(Disaggregation = HHHEthnicity)

# Combine the tables for all the disaggregations
BarriersToTrainingTable <- rbind(BarriersToTrainingTotal, BarriersToTrainingGender, BarriersToTrainingEthnicity) %>% 
  #round the percentage to 2 decimal place
  mutate(Percentage = round(Percentage, 2))

# Write excel
write.xlsx(BarriersToTrainingTable, "report/BarriersToTrainingTable.xlsx")

#########################################################################################################################

#CC 1.3 Treatment with respect and dignity

TreatedRespectfully <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHID, HHAsstRespect, HHDTPDign, HHHEthnicity, RespSex) %>% 
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
    TRUE ~ "No")) %>% 
  # Set Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    HHHEthnicity = "Ethnicity of Household Head",
    RespSex = "Gender of Respondent",
    HHAsstRespect = "Treated with respect",
    HHDTPDign = "Treated with dignity",
    HHAsstRespectDign = "Treated with respect and dignity")

# Calculate the percentage of people reporting being treated with respect and dignity
TreatedRespectifullyTotal <- TreatedRespectfully %>% 
  group_by(HHAsstRespectDign) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  filter(HHAsstRespectDign == "Yes") %>%
  select(HHAsstRespectDign, Percentage) %>%
  # Change Yes to Total
  mutate(HHAsstRespectDign = "Total") %>%
  rename(Disaggregation = HHAsstRespectDign)

# Calculate the indicator, disaggregated by gender of the respondent
TreatedRespectifullyGender <- TreatedRespectfully %>% 
  group_by(RespSex, HHAsstRespectDign) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(HHAsstRespectDign == "Yes") %>%
  select(RespSex, Percentage) %>%
  rename(Disaggregation = RespSex)

# Calculate the indicator, disaggregated by HHHEthnicity
TreatedRespectifullyEthnicity <- TreatedRespectfully %>% 
  group_by(HHHEthnicity, HHAsstRespectDign) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(HHAsstRespectDign == "Yes" & HHHEthnicity != "Foreigners") %>%
  select(HHHEthnicity, Percentage) %>%
  rename(Disaggregation = HHHEthnicity)

# Combine the tables for all the disaggregations
TreatedRespectfullyTable <- rbind(TreatedRespectifullyTotal, TreatedRespectifullyGender, TreatedRespectifullyEthnicity) %>% 
  #round the percentage to 2 decimal place
  mutate(Percentage = round(Percentage, 2))

# Write excel
write.xlsx(TreatedRespectfullyTable, "report/TreatedRespectfullyTable.xlsx")

#############################################################################################################################

# 2.1 Accessible Information

AccessibleInformation <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHHEthnicity, RespSex, # Disagregation variables
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
    (HHAsstKnowEnt == "Yes" & HHAsstKnowPeople == "Yes" & HHAsstRecInfo == "Yes" & HHAsstReportMisc == "Yes") ~ "Yes",
    TRUE ~ "No")) %>% 
  # Set Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    HHHEthnicity = "Ethnicity of Household Head",
    RespSex = "Gender of Respondent",
    HHAsstKnowEnt = "Know how to access the program information",
    HHAsstKnowPeople = "Know how people were chosen to receive assistance",
    HHAsstRecInfo = "Received information about the program location etc",
    HHAsstReportMisc = "Know how to report misconduct")

# Calculate the percentage of beneficiaries reporting accessible information (Total)
AccessibleInformationTotal <- AccessibleInformation %>% 
  group_by(AccessibleInformation) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  filter(AccessibleInformation == "Yes") %>%
  select(AccessibleInformation, Percentage) %>%
  # Change Yes to Total
  mutate(AccessibleInformation = "Total") %>%
  rename(Disaggregation = AccessibleInformation)

# Galculate the indicator disaggregated by the gender of the respondent
AccessibleInformationGender <- AccessibleInformation %>% 
  group_by(RespSex, AccessibleInformation) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(AccessibleInformation == "Yes") %>%
  select(RespSex, Percentage) %>%
  rename(Disaggregation = RespSex)

# Calculate the indicator, disaggregated by HHHEthnicity
AccessibleInformationEthnicity <- AccessibleInformation %>% 
  group_by(HHHEthnicity, AccessibleInformation) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(AccessibleInformation == "Yes" & HHHEthnicity != "Foreigners") %>%
  select(HHHEthnicity, Percentage) %>%
  rename(Disaggregation = HHHEthnicity)


# Combine the tables for all the disaggregations
AccessibleInformationTable <- rbind(AccessibleInformationTotal, AccessibleInformationGender, AccessibleInformationEthnicity) %>% 
  #round the percentage to 2 decimal place
  mutate(Percentage = round(Percentage, 2))

# Write excel
write.xlsx(AccessibleInformationTable, "report/AccessibleInformationTable.xlsx")

#############################################################################################################################

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
    RespGender == 0 ~ "Female")) %>% 
  # Filter out respondents below 18 years
  filter(RespAge >= 18) %>% 
  # Add variable lables
  set_variable_labels(
    interview_key = "Interview Key",
    HHID = "Household ID",
    HHMemberID = "Household Member ID",
    RespGender = "Gender of respondent",
    RespAge = "Age of respondent")

# Combine the demographic characteristics with the cross cutting data

CommunityParticipation <- CrossCuttingData %>% 
  # Select the necessary variables for this analysis
  select(interview_key, HHID, IDPoor, HHHEthnicity, # Disagregation variables
         starts_with("RGenEntity")) %>% # Indicator calculation variables
  # Join with Gender data
  left_join(GenderData, by = c("interview_key", "HHID")) %>%
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
    (RGenEntityNeg == "Yes" | RGenEntityViab == "Yes" | RGenEntityDM == "Yes") ~ "Yes",
    TRUE ~ "No")) %>% 
  # Set Variable Labels
  set_variable_labels(
    interview_key = "Interview Key",
    HHID = "Household ID",
    IDPoor = "Household IDPoor Status",
    HHHEthnicity = "Ethnicity of Household Head",
    RespGender = "Gender of Respondent",
    RespAge = "Age of Respondent",
    RGenEntityYN = "Member of AC Management Committee",
    RGenEntityRights = "Know Rights and responsibilities of AC members",
    RGenEntityMeet = "Member Consulted During Meetings",
    RGenEntityOp = "Opinion Considered During Meetings",
    RGenEntityNeg = "Negotiated with other members",
    RGenEntityViab = "Nogotiated or consulted on Viable Solutions",
    RGenEntityDM = "Participated as Decision Maker",
    CommunityParticipation = "Meaningful Participation in the AC")

# Calculate the percentage of beneficiaries reporting meaningful participation (Total)
CommunityParticipationTotal <- CommunityParticipation %>% 
  group_by(CommunityParticipation) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  filter(CommunityParticipation == "Yes") %>%
  select(CommunityParticipation, Percentage) %>%
  # Change Yes to Total
  mutate(CommunityParticipation = "Total") %>%
  rename(Disaggregation = CommunityParticipation)

# Calculate the indicator disaggregated by gender of the respondent
CommunityParticipationGender <- CommunityParticipation %>% 
  group_by(RespGender, CommunityParticipation) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(CommunityParticipation == "Yes") %>%
  select(RespGender, Percentage) %>%
  rename(Disaggregation = RespGender)


# Calculate the indicator, disaggregated by HHHEthnicity
CommunityParticipationEthnicity <- CommunityParticipation %>% 
  group_by(HHHEthnicity, CommunityParticipation) %>% 
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  filter(CommunityParticipation == "Yes" & HHHEthnicity != "Foreigners") %>%
  select(HHHEthnicity, Percentage) %>%
  rename(Disaggregation = HHHEthnicity)

# Combine the tables for all the disaggregations
CommunityParticipationTable <- rbind(CommunityParticipationTotal, CommunityParticipationGender, CommunityParticipationEthnicity) %>% 
  #round the percentage to 2 decimal place
  mutate(Percentage = round(Percentage, 2))

# Write excel
write.xlsx(CommunityParticipationTable, "report/CommunityParticipationTable.xlsx")


#####################################################END OF SCRIPT###################################################################





