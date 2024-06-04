# Load Necessary Packages -----------------------------------------------------------------

library(tidyverse)
library(labelled)
library(expss)
library(janitor)

# Defining broad household expenditure categories -------------------------------

FoodExp <- c("HHExpFCer_Purch_MN_7D", "HHExpFCer_GiftAid_MN_7D", "HHExpFCer_Own_MN_7D", # Expenditure on cereals
             "HHExpFTub_Purch_MN_7D", "HHExpFTub_GiftAid_MN_7D", "HHExpFTub_Own_MN_7D", # Expenditure on tubers
             "HHExpFPuls_Purch_MN_7D", "HHExpFPuls_GiftAid_MN_7D", "HHExpFPuls_Own_MN_7D", # Expenditure on pulses & nuts
             "HHExpFVeg_Purch_MN_7D", "HHExpFVeg_GiftAid_MN_7D", "HHExpFVeg_Own_MN_7D", # Expenditure on vegetables
             "HHExpFFrt_Purch_MN_7D", "HHExpFFrt_GiftAid_MN_7D", "HHExpFFrt_Own_MN_7D", # Expenditure on fruits
             "HHExpFAnimMeat_Purch_MN_7D", "HHExpFAnimMeat_GiftAid_MN_7D", "HHExpFAnimMeat_Own_MN_7D", # Expenditure on meat
             "HHExpFAnimFish_Purch_MN_7D", "HHExpFAnimFish_GiftAid_MN_7D", "HHExpFAnimFish_Own_MN_7D", # Expenditure on fish
             "HHExpFFats_Purch_MN_7D", "HHExpFFats_GiftAid_MN_7D", "HHExpFFats_Own_MN_7D", # Expenditure on fats
             "HHExpFDairy_Purch_MN_7D", "HHExpFDairy_GiftAid_MN_7D", "HHExpFDairy_Own_MN_7D", # Expenditure on milk/dairy products
             "HHExpFEgg_Purch_MN_7D", "HHExpFEgg_GiftAid_MN_7D", "HHExpFEgg_Own_MN_7D", # Expenditure on eggs
             "HHExpFSgr_Purch_MN_7D", "HHExpFSgr_GiftAid_MN_7D", "HHExpFSgr_Own_MN_7D", # Expenditure on sugar/confectionery/desserts
             "HHExpFCond_Purch_MN_7D", "HHExpFCond_GiftAid_MN_7D", "HHExpFCond_Own_MN_7D", # Expenditure on condiments
             "HHExpFBev_Purch_MN_7D", "HHExpFBev_GiftAid_MN_7D", "HHExpFBev_Own_MN_7D", # Expenditure on beverages
             "HHExpFOut_Purch_MN_7D", "HHExpFOut_GiftAid_MN_7D", "HHExpFOut_Own_MN_7D") # Expenditure on snacks/meals prepared outside the house


NonFoodConsumption <- c("HHExpNFHyg_Purch_MN_1M", "HHExpNFHyg_GiftAid_MN_1M", # Expenditure on hygiene products
                        #"HHExpNFTransp_Purch_MN_1M", "HHExpNFTransp_ GiftAid _MN_1M", # Expenditure on transportation
                        "HHExpNFFuel_Purch_MN_1M", "HHExpNFFuel_GiftAid_MN_1M", # Expenditure on fuel
                        "HHExpNFWat_Purch_MN_1M", "HHExpNFWat_GiftAid_MN_1M", # Expenditure on Water
                        "HHExpNFElec_Purch_MN_1M", "HHExpNFElec_GiftAid_MN_1M", # Expenditure on electricity
                        "HHExpNFEnerg_Purch_MN_1M", "HHExpNFEnerg_GiftAid_MN_1M", # Expenditure on energy
                        #"HHExpNFDwelSer_Purch_MN_1M", "HHExpNFDwelSer_GiftAid_MN_1M", # Expenditure dwelling services
                        "HHExpNFPhone_Purch_MN_1M", "HHExpNFPhone_GiftAid_MN_1M", # Expenditure on communication related services
                        "HHExpNFRecr_Purch_MN_1M", "HHExpNFRecr_GiftAid_MN_1M", # Expenditure on recreation services
                        #"HHExpNFAlcTobac_Purch_MN_1M", "HHExpNFAlcTobac_GiftAid_MN_1M") # Expenditure on alcohol and tobacco
                        )

NonFoodIntermediate <- c("HHExpNFMedServ_Purch_MN_6M", "HHExpNFMedServ_GiftAid_MN_6M", # Expenditure on medical services
                        "HHExpNFMedGood_Purch_MN_6M", "HHExpNFMedGood_GiftAid_MN_6M", # Expenditure on medical goods
                        "HHExpNFCloth_Purch_MN_6M", "HHExpNFCloth_GiftAid_MN_6M", # Expenditure on clothing and footwear
                        "HHExpNFEduFee_Purch_MN_6M", "HHExpNFEduFee_GiftAid_MN_6M", # Expenditure on education services
                        "HHExpNFEduGood_Purch_MN_6M", "HHExpNFEduGood_GiftAid_MN_6M", # Expenditure on education goods
                        "HHExpNFRent_Purch_MN_6M", "HHExpNFRent_GiftAid_MN_6M", # Expenditure on household rentals
                        "HHExpNFHHSoft_Purch_MN_6M", "HHExpNFHHSoft_GiftAid_MN_6M", # Expenditure on household non durable goods (e.g., bed sheets, blankets etc)
                        "HHExpNFHHMaint_Purch_MN_6M", "HHExpNFHHMaint_GiftAid_MN_6M") # Expenditure on household routine maintenance 

# Defining the Minimum Expenditure Basket (MEB) -----------------------------------
MEB <- 375158 # This is the new MEB for Cambodia. We might need to use the previous MEB for comparison.
SMEB <- 180648 # This is the new SMEB for Cambodia.We might need to use the previous SMEB for comparison.


# Loading data and calculating ECMEN --------------------------------------------
complete_data <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx") %>% 
  # Select relevant columns to calculate ECMEN
  select(ADMIN4Name, ACName, HHID, HHList, HHBaseline, 
         starts_with("HHExp"), HHHEthnicity, HHHLanguage,
         IDPoor, HHHSex) %>% 
  # Change HHBaseline, HHHEthnicity, HHHLanguage, IDPoor, HHHSex to factors
  mutate(HHBaseline = as_factor(HHBaseline),
         HHHEthnicity = as_factor(HHHEthnicity),
         HHHLanguage = as_factor(HHHLanguage),
         IDPoor = as_factor(IDPoor),
         HHHSex = as_factor(HHHSex)) %>%
  # Assign labels to factor variables categories
  mutate(HHBaseline = case_when(
        HHBaseline == 0 ~ "Baseline",
        HHBaseline == 1 ~ "Not Baseline",
        TRUE ~ "Don't Know",),
        HHHEthnicity = case_when(
        HHHEthnicity == 1 ~ "Khmer",
        TRUE ~ "Non Khmer"),
        HHHLanguage = case_when(
        HHHLanguage == 1 ~ "Khmer",
        HHHLanguage == 2 ~ "Bunong",
        TRUE ~ "Other"),
        IDPoor = case_when(
        IDPoor == 0 ~ "No",
        IDPoor == 1 ~ "Yes (Valid)",
        IDPoor == 2 ~ "Yes (Not Valid)"),
        HHHSex = case_when(
        HHHSex == 0 ~ "Female",
        HHHSex == 1 ~ "Male")) %>%
  # mutate a variable by summing across variables that contains _7
  mutate(TotalFoodExp = rowSums(across(contains("_7"))),
         TotalNonFoodExp = rowSums(across(contains("_1M"))),
         TotalNonFoodIntExp = rowSums(across(contains("_6M")))) %>% 
  # Convert TotalFoodExp to monthly by dividing by 7 and multiplying by 30
  mutate(TotalFoodExp = TotalFoodExp * 30/7) %>%
  # Convert TotalNonFoodIntExp to monthly by dividing by 6
  mutate(TotalNonFoodIntExp = TotalNonFoodIntExp / 6) %>%
  # Convert add the total food and non food expenditure. Perform the calculation row wise
  mutate(TotalExp = rowSums(across(c(TotalFoodExp, TotalNonFoodIntExp, TotalNonFoodExp)))) %>% 
  # Convert the total expenditure to per capita values (Economic Capacity) i.e., by dividing by the number of household members
  mutate(TotalExpPerCapita = TotalExp / HHList) %>%
  # Create the ECMEN variable by comparing the TotalExpPerCapita by the Minimum Expenditure Basket (MEB)
  mutate(ECMEN = case_when(
    TotalExpPerCapita >=  MEB ~ "Able to meet essential needs",
    TotalExpPerCapita < MEB ~ "Unable to meet essential needs"
  )) %>%
  # Calculate survival ecmen
  mutate(SurvivalECMEN = case_when(
    TotalExpPerCapita >=  SMEB ~ "Able Survive",
    TotalExpPerCapita < SMEB ~ "Unable to Survive"
  ))

# Compute the percentage of households that are able to meet essential needs

complete_data %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity) %>%
  count(ECMEN) %>% 
  mutate(Percentage = 100 * n / sum(n))
  
# Compute the percentage of households that are able to meet survival needs 
complete_data %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity, HHHLanguage) %>%
  count(SurvivalECMEN) %>% 
  mutate(Percentage = 100 * n / sum(n))

# Compute the average economic capacity of households

complete_data %>% 
  # Group by HHHEthnicity, HHHLanguage, IDPoor, HHHSex
  group_by(HHHEthnicity, HHHLanguage, IDPoor, HHHSex) %>%
  summarise(AvgECMEN = mean(TotalExpPerCapita, na.rm = TRUE))
