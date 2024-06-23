
library(tidyverse)
library(labelled)
library(expss)
library(janitor)
library(gt)
library(readxl)
library(extrafont)


# Source the functions.R file
#source("~/Work 2023 -Eltone/General M&E/GASFP Project/gasfp-outcome-survey/code/functions.R")

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
NewMEB <- 375158 # This is the new MEB for Cambodia. 
NewSMEB <- 180648 # This is the new SMEB for Cambodia.
OldMEB <- 323614 # This is the old MEB for Cambodia. 
OldSMEB <- 159181 # This is the old SMEB for Cambodia.

  
# Loading data and calculating ECMEN --------------------------------------------
ECMENdata <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage, 
         starts_with("HHExp")) %>%
  # mutate a variable by summing across variables that contains _7
  mutate(TotalFoodExp = rowSums(across(ends_with("_7D"))),
         TotalNonFoodExp = rowSums(across(ends_with("_1M"))),
         TotalNonFoodIntExp = rowSums(across(ends_with("_6M")))) %>% 
  # Convert TotalFoodExp to monthly by dividing by 7 and multiplying by 30
  mutate(TotalFoodExp = TotalFoodExp * 30/7) %>%
  # Convert TotalNonFoodIntExp to monthly by dividing by 6
  mutate(TotalNonFoodIntExp = TotalNonFoodIntExp / 6) %>%
  # Convert add the total food and non food expenditure. Perform the calculation row wise
  mutate(TotalExp = rowSums(across(c(TotalFoodExp, TotalNonFoodIntExp, TotalNonFoodExp)))) %>% 
  # Convert the total expenditure to per capita values (Economic Capacity) i.e., by dividing by the number of household members
  mutate(TotalExpPerCapita = TotalExp / HHList) %>%
  # Convert the TotalExpPerCapita to USD
  mutate(TotalExpPerCapitaUSD = TotalExpPerCapita / 4100) %>%
  distinct(interview_key, .keep_all = TRUE) %>%
  # Create the ECMEN variable by comparing the TotalExpPerCapita by the Minimum Expenditure Basket (MEB)
  mutate(ECMEN = case_when(
    TotalExpPerCapita >=  OldMEB ~ "Able to meet essential needs",
    TotalExpPerCapita < OldMEB ~ "Unable to meet essential needs"
  )) %>%
  # Calculate survival ecmen
  mutate(SurvivalECMEN = case_when(
    TotalExpPerCapita >=  OldSMEB ~ "Able Survive",
    TotalExpPerCapita < OldSMEB ~ "Unable to Survive"
  ))

############################################################END OF DATA CLEANING##############################################################################

# Indicator 1: Overall Percentage of households that are unable to meet essential needs

OveralECMEN <- ECMENdata %>% 
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) #%>% 
  # Filter out the households that are not able to meet essential needs
  #filter(ECMEN == "Unable to meet essential needs")
  

# Indicator 2: Percentage of households that are unable to meet essential needs, dis aggregated gender of the household heard

HHHSexECMEN <- ECMENdata %>% 
  group_by(HHHSex) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) #%>% 
  # Filter out the households that are not able to meet essential needs
  filter(ECMEN == "Unable to meet essential needs") #%>% 
  # Pivot wider
  pivot_wider(names_from = HHHSex, 
              values_from = Percentage)
  
  # Indicator 3: Percentage of households that are unable to meet essential needs, dis aggregated by ethnicity
  
  HHHEthnicityECMEN <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
    # Filter out the households that are not able to meet essential needs
    filter(ECMEN == "Unable to meet essential needs")
  
  # Indicator 4: Average economic per capita capacity 
  ECMENIncTot <- ECMENdata %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2))
  
  # Indicator 5: Average economic per capita capacity, dis aggregated by gender of the household head
  
  ECMENIncHHHSex <- ECMENdata %>%
    group_by(HHHSex) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2))
  
  # Indicator 6: Average economic per capita capacity, dis aggregated by Ethinicity of the household head
  ECMENIncHHHEthnicity <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2))
  
############################################################END OF INDICATOR CALCULATION########################################  

## INDICATOR VISUALISATIONS
  
  HHHSexECMEN %>% 
  ggplot(aes(ECMEN, Percentage)) +
  geom_col(width = 0.5) + 
  facet_wrap(~HHHSex) +
  labs(title = "Female headed households are more likely to be unable to meet essential", 
       x = "Economic Capacity To Meet Essential Needs", 
       y = "Percentage of Households") +
  # Add margins to the labs and the title of the graph
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(margin = margin(t = 20)),
        axis.text.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text(margin = margin(b = 30, t = 30),
                                  color = "#2A93FC",
                                  size = 16),
        strip.text.x = element_text(margin = margin(b = 10, t = 10),
                                    size = 16)) +
  # Add a theme
  theme_classic()
  # Select the facet wrap element and make it bigger


ECMENdata %>% 
  ggplot(aes(HHHSex, ECMEN)) +
  geom_count()

ECMENdata %>%
  filter(TotalExpPerCapitaUSD < 500) %>%
  ggplot(aes(HHList, TotalExpPerCapitaUSD,
             color = HHHSex)) +
  geom_point(position = "jitter")
  


