library(tidyverse)
library(RM.weights)

fiesVars <- c("FIESWorried", "FIESHealthy", "FIESSkipped", "FIESRanOut", "FIESWholeDay", "FIESFewFoods", "FIESHungry", "FIESAteLess")
#Load data and select necessary variables
fiesData <- Copy_of_Data_Format_WFP_GASFP_WO8 %>% #Add the data file path here
  #Select necessary variables
  select(ADMIN4Name, ACName, HHID, HHList, HHBaseline,HHHEthnicity, HHHLanguage,
         IDPoor, HHHSex, starts_with("FIES"))

##Analyse data

fiesData %>% 
  #Find the mean of 1's "Yes"
  summarise(TotalWorried =  mean(FIES_Worried, na.rm = T),
            TotalHealthy = mean(FIES_Healthy, na.rm = T),
            TotalFewFoods = mean(FIES_FewFoods, na.rm = T),
            TotalSkipped = mean(FIES_Skipped, na.rm = T),
            TotalAteLess = mean(FIES_AteLess, na.rm = T),
            TotalRanOut = mean(FIES_RanOut, na.rm = T),
            TotalHungry = mean(FIES_Hungry, na.rm = T),
            TotalWholeDay = mean(FIES_WholeDay, na.rm = T)) 
 
  
numericVariables <- function(variable) {
  mutate(variable = case_when(as.numeric(variable > 1, NA, T)))
  variable
}
