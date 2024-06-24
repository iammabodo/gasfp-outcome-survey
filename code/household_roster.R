library(tidyverse)
library(readxl)
library(openxlsx)

# This module to load the household roster data and clean it for analysis and merge with the disability and agriculture production data
# Loading From The Master File --------------------------------------------
HHRoster <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, SEX_Resp, HHHEthnicity, HHHLanguage, IDPoor, SEX_HHH, # Identification variables 
         starts_with("HHExp"), # Variables to calculate ECMEN
         contains("SAMSPHL"), # Agricultural Production variables
         starts_with("HHAsst"), starts_with("HHDTP"), starts_with("RGenEntity"), # Cross cutting variables
         RFinancSitGender, HHGenMembers, RFinancSit, RFinancSitRsn, LadderToday, Ladder1YearAgo, LadderReason, # Economic Empowerment variables
         starts_with("FIES"), # FIES Indicator
         starts_with("HHCRCS"), # Climate Resilience Capacity Score Variables
         starts_with("LcsEN"), #Livelihoods Coping Strategy indicator
         ) %>%  
  # Remove columns that are not needed
  select(-c(HHCRCSShocks, HHCRCSFloods, HHCRCSWildFire, HHCRCSHeatWave, HHCRCSStorms, HHCRCSDroughts), # Unnecessary Resilience variablea
         -(contains("LcsENAccess__"))) %>%  # Unnecessary Coping Strategy variables
  # Assign labels to grouping variables categories
  mutate(HHBaseline = case_when(
    HHBaseline == 1 ~ "Baseline Members",
    HHBaseline == 0 ~ "New Members",
    TRUE ~ "Don't Know",),
    HHHEthnicity = case_when(
      HHHEthnicity == 1 ~ "Ethnic Majority",
      HHHEthnicity == 2  | HHHEthnicity == 4  | HHHEthnicity == 5 | 
                           HHHEthnicity == 6 | HHHEthnicity == 7 | HHHEthnicity == 8 | 
                           HHHEthnicity == 9 | HHHEthnicity == 10 ~ "Ethnic Minority",
      TRUE ~ "Foreigners"),
    HHHLanguage = case_when(
      HHHLanguage == 1 ~ "Khmer",
      HHHLanguage == 2 ~ "Bunong",
      TRUE ~ "Other"),
    IDPoor = case_when(
      IDPoor == 1 ~ "Yes",
      TRUE ~ "No"),
    HHHSex = case_when(
      SEX_HHH == 0 ~ "Female",
      SEX_HHH == 1 ~ "Male"),
    RespSex = case_when(
      SEX_Resp == 0 ~ "Female",
      SEX_Resp == 1 ~ "Male"),
    ADMIN4Name = case_when(
      ADMIN4Name == 100 ~ "Nang Khi Loek",
      ADMIN4Name == 200 ~ "Ou Buon Leu",
      ADMIN4Name == 300 ~ "Roya",
      ADMIN4Name == 400 ~ "Sokh Sant",
      ADMIN4Name == 500 ~ "Sre Huy",
      ADMIN4Name == 600 ~ "Sre Sangkom",
      TRUE ~ "Other"),
    ACName = case_when(
      ACName == 1 ~ "Phum Srae Huy",
      ACName == 2 ~ "Samak Mean Rith Rung Roeung",
      ACName == 3 ~ "Samaki Phum Toul",
      ACName == 4 ~ "Apiwat Mean Chey",
      ACName == 5 ~ "Samaki Rik Chom Roeung")) %>%
  # Mutate HHHSex to be equal to SEX_Resp if it is missing
  mutate(HHHSex = if_else(is.na(HHHSex), RespSex, HHHSex)) %>% 
  select(interview_key, HHID, HHList, HHBaseline, HHHEthnicity, HHHLanguage, IDPoor, HHHSex, RespSex, ADMIN4Name, everything(),
         -c(SEX_HHH, SEX_Resp)) %>% 
  # Change everything that is character to factor
  mutate_if(is.character, as.factor) %>%
  # # Pivot longer using PSAMSPHLCommN_1 and PSAMSPHLCommN_2 variables
  pivot_longer(cols = c("PSAMSPHLCommN_1", "PSAMSPHLCommN_2"),
               names_to = "RiceType",
               values_to = "Produced") %>%
  # Mutate the RiceType variable to be more descriptive
  mutate(RiceType = case_when(RiceType == "PSAMSPHLCommN_1" ~ "Organic Rice",
                              RiceType == "PSAMSPHLCommN_2" ~ "Non Organic Rice")) %>%
  # Mutate the Produced variable to be more descriptive
  mutate(Produced = case_when(Produced == 1 ~ "Yes",
                              TRUE ~ "No"))

# Load the household roster file and determine wether the family has a person with disability and how many members
HHDisabRoster <- read_excel("data/HHRoster_Cleaned_Numeric.xlsx") %>% 
  select(interview_key, HHRoster_id, HHFulName, HHMemsex, HHMemAge, contains("Disab")) %>% 
  mutate(PDisabSee = case_when(
    PDisabSee == 1 ~ "No difficulty",
    PDisabSee == 2 ~ "Some difficulty",
    PDisabSee == 3 ~ "A lot of difficulty",
    PDisabSee == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  DisabHear = case_when(
    DisabHear == 1 ~ "No difficulty",
    DisabHear == 2 ~ "Some difficulty",
    DisabHear == 3 ~ "A lot of difficulty",
    DisabHear == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabWalk = case_when(
    PDisabWalk == 1 ~ "No difficulty",
    PDisabWalk == 2 ~ "Some difficulty",
    PDisabWalk == 3 ~ "A lot of difficulty",
    PDisabWalk == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabRemember = case_when(
    PDisabRemember == 1 ~ "No difficulty",
    PDisabRemember == 2 ~ "Some difficulty",
    PDisabRemember == 3 ~ "A lot of difficulty",
    PDisabRemember == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabUnderstand = case_when(
    PDisabUnderstand == 1 ~ "No difficulty",
    PDisabUnderstand == 2 ~ "Some difficulty",
    PDisabUnderstand == 3 ~ "A lot of difficulty",
    PDisabUnderstand == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabWash = case_when(
    PDisabWash == 1 ~ "No difficulty",
    PDisabWash == 2 ~ "Some difficulty",
    PDisabWash == 3 ~ "A lot of difficulty",
    PDisabWash == 4 ~ "Cannot do at all",
    TRUE ~ NA)) %>% 
  # Mutate a variable to show that a member is disabled if they have a disability in any of the 6 categories
  mutate(Disab = case_when(
    PDisabSee == "Some difficulty" | PDisabSee == "A lot of difficulty" | PDisabSee == "Cannot do at all" |
      DisabHear == "Some difficulty" | DisabHear == "A lot of difficulty" | DisabHear == "Cannot do at all" |
      PDisabWalk == "Some difficulty" | PDisabWalk == "A lot of difficulty" | PDisabWalk == "Cannot do at all" |
      PDisabRemember == "Some difficulty" | PDisabRemember == "A lot of difficulty" | PDisabRemember == "Cannot do at all" |
      PDisabUnderstand == "Some difficulty" | PDisabUnderstand == "A lot of difficulty" | PDisabUnderstand == "Cannot do at all" |
      PDisabWash == "Some difficulty" | PDisabWash == "A lot of difficulty" | PDisabWash == "Cannot do at all" ~ "Yes",
    TRUE ~ "No")) %>% 
  # Mutate HHMemsex to be equal to Female if 0 or Male if 1
  mutate(HHMemsex = case_when(
    HHMemsex == 0 ~ "Female",
    HHMemsex == 1 ~ "Male")) %>%
  # Group by the household ID and count the number of disabled members
  group_by(interview_key) %>%
  summarise(NumDisab = sum(Disab == "Yes", na.rm = TRUE)) %>%
  # Mutate a variable to show if the household has a disabled member
  mutate(HHDisab = case_when(
    NumDisab > 0 ~ "Yes",
    TRUE ~ "No"))


# Load the agricultural production rosters
# Import first roster data
PSAMSRiceRoster <- read_excel("data/Roster_PSAMSRice_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key, Roster_PSAMSRice_id, 
         PSAMSRiceHarvestsNmb, PSAMSNutCropIncr, PSAMSPHLCommEnough,
         PSAMSRiceInputsMN, PSAMSRiceSell, PSAMSRiceSellTime, PSAMSRiceSellQuant,
         PSAMSRiceSellMN, PSAMSRiceRevenue, PSAMSRiceIncome, Income) %>% 
  # Mutate variables to have more descriptive values
  mutate(PSAMSNutCropIncr = case_when(
    PSAMSNutCropIncr == 1 ~ "More",
    PSAMSNutCropIncr == 2 ~ "Less",
    PSAMSNutCropIncr == 3 ~ "The Same",
    TRUE ~ "Not Applicable"),
    PSAMSPHLCommEnough = case_when(
      PSAMSPHLCommEnough == 1 ~ "Yes",
      TRUE ~ "No"),
    PSAMSRiceSell = case_when(
      PSAMSRiceSell == 1 ~ "Yes",
      PSAMSRiceSell == 0 ~ "No",
      TRUE ~ "Don't Know"),
    RiceType = case_when(
      Roster_PSAMSRice_id == 1 ~ "Organic Rice",
      Roster_PSAMSRice_id == 2 ~ "Non Organic Rice")) %>% 
  # Change PSAMSRiceSellMN, PSAMSRiceSellQuant, PSAMSRiceInputsMN, PSAMSRiceRevenue, PSAMSRiceIncome, Income to numeric
  mutate(PSAMSRiceSellMN = as.numeric(PSAMSRiceSellMN),
         PSAMSRiceSellQuant = as.numeric(PSAMSRiceSellQuant),
         PSAMSRiceInputsMN = as.numeric(PSAMSRiceInputsMN),
         PSAMSRiceRevenue = as.numeric(PSAMSRiceRevenue),
         PSAMSRiceIncome = as.numeric(PSAMSRiceIncome),
         Income = as.numeric(Income))


# Import second roster data
PSAMSHarvestRoster <- read_excel("data/Roster_HarvestNumb_Cleaned_Numeric.xlsx") %>% 
  # Selecting required columns
  select(interview_key, Roster_PSAMSRice_id, Roster_HarvestNmb_id, 
         PSAMSPHLCommArea, PSAMSPHLCommArea_Unit, PSAMSPHLCommArea_Unit_OTH, PSAMSPHLCommQuant,
         PSAMSPHLCommQntHand, PSAMSPHLCommQntLost) %>% 
  # Rename PSAMSPHLCommArea_Unit to have more descriptive values
  mutate(PSAMSPHLCommArea_Unit = case_when(
    PSAMSPHLCommArea_Unit == 1 ~ "Square Meter",
    PSAMSPHLCommArea_Unit == 2 ~ "Acre",
    PSAMSPHLCommArea_Unit == 3 ~ "Kong",
    PSAMSPHLCommArea_Unit == 4 ~ "Hectare",
    TRUE ~ "Other"),
    RiceType = case_when(
      Roster_PSAMSRice_id == 1 ~ "Organic Rice",
      Roster_PSAMSRice_id == 2 ~ "Non Organic Rice")) %>% 
  # Change PSAMSPHLCommArea, PSAMSPHLCommQuant, PSAMSPHLCommQntHand, PSAMSPHLCommQntLost to numeric
  mutate(PSAMSPHLCommArea = as.numeric(PSAMSPHLCommArea),
         PSAMSPHLCommQuant = as.numeric(PSAMSPHLCommQuant),
         PSAMSPHLCommQntHand = as.numeric(PSAMSPHLCommQntHand),
         PSAMSPHLCommQntLost = as.numeric(PSAMSPHLCommQntLost)) %>% 
  # Chnage PSAMSPHLCommArea to hectare
  mutate(PSAMSPHLCommArea = if_else(PSAMSPHLCommArea_Unit == "Acre", PSAMSPHLCommArea * 0.01, PSAMSPHLCommArea)) %>% 
  # Change PSAMSPHLCommArea to hectare if the unit is in square meter
  mutate(PSAMSPHLCommArea = if_else(PSAMSPHLCommArea_Unit == "Square Meter", PSAMSPHLCommArea * 0.0001, PSAMSPHLCommArea)) %>%
  # Calculate the percentage of post harvest losses
  mutate(PSAMSPHLCommQntLostPerc = (PSAMSPHLCommQntLost / PSAMSPHLCommQuant) * 100) %>% 
  # Round to 2 significant digits
  mutate(PSAMSPHLCommQntLostPerc = round(PSAMSPHLCommQntLostPerc, 2))


# Now we have all the data we need to calculate all the indicators. Now let us merge all the different data sets, starting with the household roster

# Merge the household roster with the disability roster
FullHHRoster <- left_join(HHRoster, HHDisabRoster, by = "interview_key") %>% 
  # Merge by rice type and interview key
  left_join(PSAMSRiceRoster, by = c("interview_key", "RiceType")) %>% 
  # Merge by rice type and interview key
  left_join(PSAMSHarvestRoster, by = c("interview_key", "RiceType"))



# Save the data
write.xlsx(FullHHRoster, "data/FullHHRosterClean.xlsx")


###################################END OF CODE###################################







