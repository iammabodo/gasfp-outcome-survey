# Load necessary packages

library(tidyverse)
library(readxl)
library(janitor)
library(gt)
library(openxlsx)
library(forcats)

    
# Import the data

EconomicEmpowermentData <- read_excel("data/WFP_GASFP_WO8_Cleaned_Numeric.xlsx") %>% 
  # Select relevant columns to calculate the percentage of men and women reporting to have economic empowerment
  select(interview_key, HHID, Sex, HHHEthnicity, HHHLanguage, IDPoor, HHBaseline, # Dis aggregation variables
        HHGenMembers, RFinancSit, RFinancSitRsn, LadderToday, Ladder1YearAgo, LadderReason) %>% # Indicator calculation variables
  # Mutate the variable to have meaningful labels
  mutate(Sex = case_when(
    Sex == 0 ~ "Female",
    Sex == 1 ~ "Male"),
    HHHEthnicity = case_when(
      HHHEthnicity == 1 ~ "Ethnic Majority",
      HHHEthnicity == 2  | HHHEthnicity == 4  | HHHEthnicity == 5 | 
      HHHEthnicity == 6 | HHHEthnicity == 7 | HHHEthnicity == 8 | 
      HHHEthnicity == 9 | HHHEthnicity == 10 ~ "Ethnic Minority",
      TRUE ~ NA),
    HHBaseline = case_when(
    HHBaseline == 0 ~ "Baseline Members",
    HHBaseline == 1 ~ "New Members",
    TRUE ~ "Don't Know"),
    HHHLanguage = case_when(
    HHHLanguage == 1 ~ "Khmer",
    HHHLanguage == 2 ~ "Bunong",
    TRUE ~ "Other"),
    IDPoor = case_when(
    IDPoor == 1 ~ "Yes",
    TRUE ~ "No"),
    RFinancSit = case_when(
    RFinancSit == 1 ~ "Improved",
    RFinancSit == 2 ~ "Stayed the same",
    RFinancSit == 3 ~ "Worsened",
    TRUE ~ "Prefer not to answer")) %>%
  # Create the economic empowerment variable. This is the key indicator for the analysis
  mutate(EconomicEmpowerment = case_when(
    RFinancSit == "Improved" & Ladder1YearAgo <= LadderToday ~ "Economically Empowered",
    TRUE ~ "Not Economically Empowered")) %>% 
  # Change all character variables to factors
  mutate_if(is.character, as.factor) %>% 
  # Drop NAs
  drop_na(interview_key)

##########################################################END OF DATA CLEANING###########################################################

# Calculate the percentage of people reporting economic empowerment dis aggregated by gender

GenderEconomicEmpGap <- EconomicEmpowermentData %>% 
  group_by(Sex, EconomicEmpowerment) %>% # To include the disaggregation by gender of the respondent here once we have the final data
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>% 
  ungroup() %>%
  # round the percentage to 2 decimal places
  mutate(Percentage = round(Percentage, 2))%>% 
  rename(Disagregation = Sex)

# Perform a proportion test to determine if the proportion of economically empowered individuals is different
countsGender <- EconomicEmpowermentData %>%
  group_by(Sex) %>%
  summarise(
    EconomicallyEmpowered = sum(EconomicEmpowerment == "Economically Empowered"),
    Total = n()
  )

# Perform the proportion test
propTestResultGender <- prop.test(
  x = countsGender$EconomicallyEmpowered, # Counts of "Economically Empowered" individuals
  n = countsGender$Total,                 # Total counts of individuals for each gender
  alternative = "two.sided"         # Two-sided test (default)
)

# Output the proportion test result
print(propTestResultGender)

## The results shows that the difference is not statistically significant at all acceptable levels of significance

##########################################################################################################################

# Calculate the percentage of people reporting economic empowerment disaggregated by baseline members and new members

GenderEconomicEmpGapBaseline <- EconomicEmpowermentData %>% 
  group_by(HHBaseline, EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>% 
  filter(HHBaseline != "Don't Know")%>% 
  rename(Disagregation = HHBaseline)

# Perform a proportion test to determine if the proportion of economically empowered individuals is different
countsBaseline <- EconomicEmpowermentData %>%
  group_by(HHBaseline) %>%
  filter(HHBaseline != "Don't Know") %>%
  summarise(
    EconomicallyEmpowered = sum(EconomicEmpowerment == "Economically Empowered"),
    Total = n()
  )

# Perform the proportion test
propTestResultBaseline <- prop.test(
  x = countsBaseline$EconomicallyEmpowered, # Counts of "Economically Empowered" individuals
  n = countsBaseline$Total,                 # Total counts of individuals for each gender
  alternative = "two.sided"         # Two-sided test (default)
)

# Output the proportion test result
print(propTestResultBaseline)

## This test shows that the difference is not statistically significant at all acceptable levels of significance
#############################################################################################################################

# Calculate the percentage of people reporting economic empowerment dis aggregated by ethnicity 

GenderEconomicEmpGapEthnicity <- EconomicEmpowermentData %>%
  group_by(HHHEthnicity, EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  rename(Disagregation = HHHEthnicity) %>% 
  filter(Disagregation != "NA")

# Perform a proportion test to determine if the proportion of economically empowered individuals is different
countsEthnicity <- EconomicEmpowermentData %>%
  group_by(HHHEthnicity) %>%
  filter(HHHEthnicity != "Foreigners") %>%
  summarise(
    EconomicallyEmpowered = sum(EconomicEmpowerment == "Economically Empowered"),
    Total = n()
  )

# Perform the proportion test
propTestResultEthnicity <- prop.test(
  x = countsEthnicity$EconomicallyEmpowered, # Counts of "Economically Empowered" individuals
  n = countsEthnicity$Total,                 # Total counts of individuals for each gender
  alternative = "two.sided"         # Two-sided test (default)
)

# Output the proportion test result
print(propTestResultEthnicity)

## This test shows that the difference is statistically significant (at 1% level of significance)

#############################################################################################################################

GenderEconomicEmpGapTot <- EconomicEmpowermentData %>%
  group_by(EconomicEmpowerment) %>% # To include the disaggregation
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  mutate(Disagregation = "Total") %>% 
  select(Disagregation, EconomicEmpowerment, Count, Percentage)


# Perform the proportion test
propTestResultTot <- prop.test(
  x = GenderEconomicEmpGapTot$Count[GenderEconomicEmpGapTot$EconomicEmpowerment == "Economically Empowered"],
  n = sum(GenderEconomicEmpGapTot$Count),
  p = 0.5,  # Null hypothesis proportion (50%)
  alternative = "two.sided"
)

print(propTestResultTot)

# The resuts shows that the difference is statistically significant at all acceptable levels of significance

############################################################################################################################

# Combine the three tables into one

GenderEconomicEmpGapFull <- bind_rows(GenderEconomicEmpGap, 
                                      GenderEconomicEmpGapBaseline, 
                                      GenderEconomicEmpGapEthnicity, 
                                      GenderEconomicEmpGapTot)


# Write this into an xlsx file
write.xlsx(GenderEconomicEmpGapFull, "report/GenderEconomicEmpGapFull.xlsx")

#####################################################INDICATOR CALCULATION##################################################

GenderEconomicEmpGapFull %>%
  filter(EconomicEmpowerment == "Economically Empowered") %>%
  fct_reorder(EconomicEmpowerment, Percentage) %>% 
  ggplot(aes(x = EconomicEmpowerment, y = Percentage)) +
  geom_col(aes(fill = Disagregation), position = position_dodge(width = 0.9), width = 0.5) +
  geom_text(aes(label = scales::percent(Percentage / 100), 
                group = Disagregation), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            color = "white") +
  scale_fill_brewer(palette = "Set3", name = "Disaggregation") +
  labs(title = "Gender Economic Empowerment Breakdown in Kho Nhek District",
       x = "Economic Empowerment",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
  
  
  



