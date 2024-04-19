library(tidyverse)
library(RM.weights)

fiesVars <- c("FIESWorried", "FIESHealthy", "FIESSkipped", "FIESRanOut", "FIESWholeDay", "FIESFewFoods", "FIESHungry", "FIESAteLess")
#Load data and select necessary variables
fiesData <- read_csv() %>% #Add the data file path here
  #Select necessary variables
  select(hhid, Gender, Enthinicity, Language, Gender, ACName,
         FIESWorried, FIESHealthy, FIESFewFoods, FIESSkipped,
         FIESAteLess, FIESRanOut, FIESHungry, FIESWholeDay) %>%
  #Change the "Don't Know" and "refused to answer" responses to NAs
  mutate(FIESWorried = case_when(as_numeric(FIESWorried > 1 ~ NA, T)),
         FIESHealthy = case_when(as_numeric(FIESHealthy > 1 ~ NA, T)),
         FIESFewFoods = case_when(as_numeric(FIESFewFoods > 1 ~ NA, T)),
         FIESSkipped = case_when(as_numeric(FIESSkipped > 1 ~ NA, T)),
         FIESAteLess = case_when(as_numeric(FIESAteLess > 1 ~ NA, T)),
         FIESRanOut = case_when(as_numeric(FIESRanOut > 1 ~ NA, T)),
         FIESHungry = case_when(as_numeric(FIESHungry > 1 ~ NA, T),
         FIESWholeDay = case_when(as_numeric(FIESWholeDay > 1 ~ NA, T)))) %>% 
  #Drop NAs from the FIES_ variables
  drop_na(any_of(FIESWorried, FIESHealthy, FIESSkipped, FIESRanOut, FIESWholeDay, FIESFewFoods, FIESHungry, FIESAteLess))

##Analyse data

fiesData %>% 
  #Group by dissaggregation variables
  group_by(Gender, Ethnicity, Language, IDPoor) %>% 
  #Find the sum of 1's "Yes"
  summarise(TotalWorried =  sum(FIESWorried, na.rm = T),
            TotalHealthy = sum(FIESHealthy, na.rm = T),
            TotalFewFoods = sum(FIESFewFoods, na.rm = T),
            TotalSkipped = summ(FIESSkipped, na.rm = T),
            TotalAteLess = sum(FIESAteLess, na.rm = T),
            TotalRanOut = sum(FIESRanOut, na.rm = T),
            TotalHungry = sum(FIESHungry, na.rm = T),
            TotalWholeDay = sum(FIESWholeDay, na.rm = T)) 
  
  
numericVariables <- function(variable) {
  mutate(variable = case_when(as.numeric(variable > 1, NA, T)))
  variable
}
