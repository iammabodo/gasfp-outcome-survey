library(tidyverse)
library(readxl)


BaselineRiceData <- read_excel("data/WFP- GASFP Baseline_Clean Data_V5.xlsx", sheet = 2) %>% 
  select(HHID_Unique, HH_No, PSAMSRiceSellMN, PSAMSRiceSellMNUnit, PSAMSRiceSellMN,PSAMSRiceQuant, 
         PSAMSRiceQuantUnit, PSAMSRiceSell, PSAMSRiceSellQuant, PSAMSRiceSellQuantUnit,
         PSAMSRiceSellMNUnit, PSAMSRiceInputsMN) %>% 
  distinct(HHID_Unique, .keep_all = TRUE) %>%
  drop_na(PSAMSRiceSellMN, PSAMSRiceQuant, PSAMSRiceSell, PSAMSRiceSellQuant, PSAMSRiceInputsMN) %>% 
  filter(HH_No == 1 & PSAMSRiceSellQuantUnit <10) %>%
  filter(PSAMSRiceSellMNUnit < 10) %>% 
  # Change the units to character variables 
  mutate(PSAMSRiceSellQuantUnit = case_when(
    PSAMSRiceSellQuantUnit == 1 ~ "gramms",
    PSAMSRiceSellQuantUnit == 2 ~ "kilograms",
    PSAMSRiceSellQuantUnit == 3 ~ "tons",
    PSAMSRiceSellQuantUnit == 4 ~ "bags (25kg)",
    PSAMSRiceSellQuantUnit == 5 ~ "bags (50kg)",
    PSAMSRiceSellQuantUnit == 6 ~ "bags (100kg)"
  )) %>% 
  # Change PSAMSRiceSellQuant to killograms for depending on PSAMSRiceSellQuantUnit
  mutate(NewPSAMSRiceSellQuant = case_when(
    PSAMSRiceSellQuantUnit == "gramms" ~ PSAMSRiceSellQuant / 1000,
    PSAMSRiceSellQuantUnit == "tons" ~ PSAMSRiceSellQuant * 1000,
    PSAMSRiceSellQuantUnit == "bags (25kg)" ~ PSAMSRiceSellQuant * 25,
    PSAMSRiceSellQuantUnit == "bags (50kg)" ~ PSAMSRiceSellQuant * 50,
    PSAMSRiceSellQuantUnit == "bags (100kg)" ~ PSAMSRiceSellQuant * 100,
    TRUE ~ PSAMSRiceSellQuant
  )) %>%
  # Change PSAMSRiceQuant to killograms for depending on PSAMSRicSellQuantUnit
  mutate(NewPSAMSRiceQuant = case_when(
    PSAMSRiceSellQuantUnit == "gramms" ~ PSAMSRiceQuant / 1000,
    PSAMSRiceSellQuantUnit == "tons" ~ PSAMSRiceQuant * 1000,
    PSAMSRiceSellQuantUnit == "bags (25kg)" ~ PSAMSRiceQuant * 25,
    PSAMSRiceSellQuantUnit == "bags (50kg)" ~ PSAMSRiceQuant * 50,
    PSAMSRiceSellQuantUnit == "bags (100kg)" ~ PSAMSRiceQuant * 100,
    TRUE ~ PSAMSRiceQuant
  )) %>% 
  # Calculate the price per kilogram
  mutate(PricePerKg = PSAMSRiceSellMN / NewPSAMSRiceSellQuant) %>% 
  # Calculate new rice income
  mutate(NewRiceIncome = PricePerKg * NewPSAMSRiceQuant) %>% 
  # Calculate the Net rice Income
  mutate(NetRiceIncome = NewRiceIncome - PSAMSRiceInputsMN)


BaselineRiceData %>% 
  summarise(MeanNetRiceIncome = mean(NetRiceIncome),
            MedianNetRiceIncome = median(NetRiceIncome),
            MinNetRiceIncome = min(NetRiceIncome),
            MaxNetRiceIncome = max(NetRiceIncome),
            SumNetRiceIncome = sum(NetRiceIncome))


######################################BASELINE GENDER AND ECONOMIC EMPOWERMENT INDICATOR###################################

OldGenderData <- read_excel("data/WFP- GASFP Baseline_Clean Data_V5.xlsx", sheet = 2) %>% 
  select(HHID_Unique, HH_No, PersonalIncome,LadderToday, Ladder1YearAgo) %>% 
  distinct(HHID_Unique, .keep_all = TRUE) %>%
  drop_na(PersonalIncome, LadderToday, Ladder1YearAgo) %>%
  mutate(PersonalIncome = case_when(
    PersonalIncome == 1 ~ "Improved",
    PersonalIncome == 2 ~ "Stayed the same",
    PersonalIncome == 3 ~ "Worsened",
    TRUE ~ "Prefer not to answer")) %>% 
  # Mutate Gender Empowermnet VAriable
  mutate(GenderEmpowerment = case_when(
    PersonalIncome == "Improved" & LadderToday >= Ladder1YearAgo ~ "Economically Empowered",
    TRUE ~ "Not Economically Empowered"))

# Calculate the percentage of people who are economically empowered
OldGenderData %>% 
  group_by(GenderEmpowerment) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = Count / sum(Count) * 100)

