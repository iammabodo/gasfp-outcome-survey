
library(tidyverse)
library(labelled)
library(expss)
library(janitor)
library(gt)
library(readxl)
library(extrafont)
library(ggtext)
library(openxlsx)

# Source the functions.R file
#source("~/Work 2023 -Eltone/General M&E/GASFP Project/gasfp-outcome-survey/code/functions.R")

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
NewMEB <- 375158 # This is the new MEB for Cambodia. 
NewSMEB <- 180648 # This is the new SMEB for Cambodia.
OldMEB <- 323614 # This is the old MEB for Cambodia. 
OldSMEB <- 159181 # This is the old SMEB for Cambodia.
OldMEBUSD <- round((OldMEB / 4100),2) # This is the old MEB in USD
OldSMEBUSD <- round((OldSMEB / 4100),2) # This is the old SMEB in USD
  
# Loading data and calculating ECMEN --------------------------------------------
ECMENdata <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(interview_key, ADMIN4Name, ACName, HHID, HHList, HHBaseline, IDPoor, HHHSex, RespSex, HHHEthnicity, HHHLanguage, 
         starts_with("HHExp")) %>%
  # Rename some of the variables we will be using
  rename(HHExpFAnimMeat_GiftAid_7D = HHExpFAnimMeat_GiftAid,
         HHExpFAnimMeat_Own_MN_7D = HHExpFAnimMeat_Own_MN,
         HHExpFAnimFish_GiftAid_MN_7D =  HHExpFAnimFish_GiftAid_MN,
         HHExpNFAlcTobac_Purch_MN_1M = HHExpNFAlcTobac_Purch_MN_1,
         HHExpNFAlcTobac_GiftAid_MN_1M = HHExpNFAlcTobac_GiftAid_MN,
         HHExpNFMedServ_GiftAid_MN_6M = HHExpNFMedServ_GiftAid_MN_6,
         HHExpNFMedGood_GiftAid_MN_6M = HHExpNFMedGood_GiftAid_MN_6,
         HHExpNFEduGood_GiftAid_MN_6M = HHExpNFEduGood_GiftAid_MN,
         HHExpNFHHSoft_GiftAid_MN_6M = HHExpNFHHSoft_GiftAid_MN_6,
         HHExpNFHHMaint_GiftAid_MN_6M = HHExpNFHHMaint_GiftAid_MN) %>%
  # mutate Total Food Expenditure, Total Non Food Expenditure and Total Non Food Intermediate Expenditure by summing across relevant variables
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
  mutate(Percentage = round(100 * n / sum(n), 2)) %>% 
  # Mutate a variable and assign a value to it which is overal ecmen
  mutate(Disagregation = "Overall") %>% 
  rename(ECMENStatus = "ECMEN") %>% 
  select(ECMENStatus, Disagregation, everything())
  

# Indicator 2: Percentage of households that are unable to meet essential needs, dis aggregated gender of the household heard

HHHSexECMEN <- ECMENdata %>% 
  group_by(HHHSex) %>%
  count(ECMEN) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2)) %>%
  rename(Disagregation = HHHSex) %>%
  rename(ECMENStatus = "ECMEN") %>% 
  select(ECMENStatus, Disagregation, everything())
  
  # Indicator 3: Percentage of households that are unable to meet essential needs, dis aggregated by ethnicity
  
  HHHEthnicityECMEN <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = HHHEthnicity) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
    filter(Disagregation != "Foreigners")
  
  
  # Indicator 4: Percentage of households that are unable to meet essential needs, dis aggregated by baseline status
 ECMENBaseline <- ECMENdata %>% 
    group_by(HHBaseline) %>%
    count(ECMEN) %>% 
    mutate(Percentage = round(100 * n / sum(n), 2))%>%
    rename(Disagregation = HHBaseline) %>%
    rename(ECMENStatus = "ECMEN") %>% 
    select(ECMENStatus, Disagregation, everything()) %>% 
   # Filter to include only people from the baseline and new members
    filter(Disagregation != "Don't Know") 
  
  # Combine the three tables into one
  ECMENIndicators <- bind_rows(OveralECMEN, HHHSexECMEN, HHHEthnicityECMEN, ECMENBaseline) %>% 
  # Change all character variables to factors
  mutate_if(is.character, as.factor)
  
  # Write this into an xlsx file
  write.xlsx(ECMENIndicators, "report/ECMENIndicators.xlsx")

##Calculate the economic capacity of the households(Avergae per capita expenditure)
  
  # Indicator 4: Average economic per capita capacity 
  ECMENIncTot <- ECMENdata %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    mutate(Disagregation = "Overall")
  
  # Indicator 5: Average economic per capita capacity, dis aggregated by gender of the household head
  
  ECMENIncHHHSex <- ECMENdata %>%
    group_by(HHHSex) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = HHHSex)
  
  # Indicator 6: Average economic per capita capacity, dis aggregated by Ethnicity of the household head
  ECMENIncHHHEthnicity <- ECMENdata %>%
    group_by(HHHEthnicity) %>%
    summarise(AvgEconomicCapacityUSD = round(mean(TotalExpPerCapitaUSD, na.rm = TRUE),2)) %>% 
    rename(Disagregation = HHHEthnicity)

# Combine the three tables into one
ECMENIncIndicators <- bind_rows(ECMENIncTot, ECMENIncHHHSex, ECMENIncHHHEthnicity) %>% select(Disagregation, AvgEconomicCapacityUSD)

# Write this into an xlsx file
write.xlsx(ECMENIncIndicators, "report/ECMENIncIndicators.xlsx")

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
                                    size = 16)) 

subtitle_text <- "Number of households <span style='color:#00BFC4'>**Male**</span> headed and 
<span style='color:#F8766D'>**Female**</span> headed" 
  
  # Select the facet wrap element and make it bigger
  ECMENdata %>% 
  filter(TotalExpPerCapitaUSD < 500) %>%
    ggplot(aes(x = ACName, 
               y = TotalExpPerCapitaUSD, 
               fill = HHHSex)) +
    geom_point(position = position_jitterdodge(), 
               size = 3,
               alpha = 0.75,
               shape = 21) + 
  geom_hline(yintercept = OldMEBUSD, 
             linetype = "dashed", 
             color = "red",
             size = 1.5) +
  geom_hline(yintercept = OldSMEBUSD, 
             linetype = "dashed", 
             color = "blue",
             size = 1.5) +
    theme_minimal(base_size = 14,
                  base_family = "Times New Roman") + # Should Learn How to use different fonts
    theme(plot.subtitle = ggtext::element_markdown(),
          legend.position = "none") + 
    labs(title = "Per Capita Expenditure",
         x = "Agriculture Cooperative Name",
         y = "Expenditure in USD",
         fill = "Gender",
         subtitle = subtitle_text)

ECMENdata %>% 
  ggplot(aes(HHHSex, ECMEN)) +
  geom_count()

ECMENdata %>%
  filter(TotalExpPerCapitaUSD < 500) %>%
  ggplot(aes(HHList, TotalExpPerCapitaUSD,
             color = HHHSex)) +
  geom_point(position = "jitter")
  


