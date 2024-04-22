library(tidyverse)

# Load the data set

RiceProduction <- read_csv("") %>%  #Input file path here
  #Select the necessary variables for this analysis - dis aggregation modules and $PSAMSRiceIncome variable
  select(hhid, PSAMSRiceIncome, PSAMSNutCropIncr, PSAMSPHLCommQntLost, PSAMSPHLCommQntHand, HHIncTot, 
         IDPoor, Gender, Ethnicity, Language, Organic)


#Indicator 01 - Total household income from household rice production
RiceIncome <- RiceProduction %>% 
  #Select relevant variables
  select(hhid, PSAMSRiceIncome, IDPoor, Gender, Ethnicity, Language, Organic) %>% 
  #Group variables by levels of dis aggregation required
  group_by(IDPoor, Gender, Ethnicity, Language, Organic) %>% 
  #Calculate the necessary descriptive statistics
  summarise(AverageRiceIncome = mean(PSAMSRiceIncome, na.rm = T),
            MedianRiceIncome = median(PSAMSRiceIncome, na.rm = T),
            TotalRiceIncome = sum(PSAMSRiceIncome, na.rm = T)) %>% 
  #Ungroup the variables - to enable further analysis
  ungroup()

#Visualize Indicator 1

RiceIncome %>% 
  ggplot(aes()) # To add necessary geoms after this line

#Indicator 02 - Percentage of households reporting increases in rice production
RiceProductionIncrease <- RiceProduction %>% 
  #Select the necessary variables to calculate this indicator
  select(hhid, SAMSNutCropIncr, IDPoor, Gender, Ethnicity, Language, Organic) %>% 
  # Group variables
  group_by(IDPoor,SAMSNutCropIncr, Gender, Ethnicity, Language, Organic) %>% 
  # Summerise to get the count
  summarise(n = n()) %>% 
  # Calculate the percentages
  summarise(percentage = n * 100/N())

# Visualize indicator 2

RiceProductionIncrease %>% 
  ggplot(aes())  # To add necessary geoms after this line


# Indicator 03 - Post-harvest losses
PostHaverstLosses <- RiceProduction %>% 
  # Select necessary variables
  select(hhid, PSAMSPHLCommQntLost, PSAMSPHLCommQntHand, IDPoor, Gender, Ethnicity, Language, Organic) %>% 
  # Create the variable to indicate the farmer had some post-harvest losses
  mutate(PHLosses = case_when(PSAMSPHLCommQntLost > 0 ~ "Incured Post Haverst Losses",
                              TRUE ~ "Did not incure post harvest losses"),
         PHLossesPcn = round((PSAMSPHLCommQntLost/PSAMSPHLCommQntHand) * 100,2)) %>% 
  # Group by necessary variables
  group_by(IDPoor, Gender, Ethnicity, Language) %>% 
  # Calculate the necessary indicators
  summerise(avgPHLosses = mean(PHLossesPcn, na.rm = TRUE))

# Visualize indicator 3

PostHaverstLosses %>% 
  ggplot(aes())  # To add necessary geoms after this line

  
  




