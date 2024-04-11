library(tidyverse)
library(RM.weights)

#Load data and select necessary variables
fiesData <- read_csv() %>% #Add the data file path here
  #Select necessary variables
  select(hhid, Gender, Enthinicity, Language, Gender, ACName,
         FIESWorried, FIESHealthy, FIESFewFoods, FIESSkipped,
         FIESAteLess, FIESRanOut, FIESHungry, FIESWholeDay) %>%
  #Change the "Don't Know" and "refused to answer" responses to NAs
  mutate(FIESWorried = case_when(FIESWorried > 1 ~ NA, T),
         FIESHealthy = case_when(FIESHealthy > 1 ~ NA, T),
         FIESFewFoods = case_when(FIESFewFoods > 1 ~ NA, T),
         FIESSkipped = case_when(FIESSkipped > 1 ~ NA, T),
         FIESAteLess = case_when(FIESAteLess > 1 ~ NA, T),
         FIESRanOut = case_when(FIESRanOut > 1 ~ NA, T),
         FIESHungry = case_when(FIESHungry > 1 ~ NA, T),
         FIESWholeDay = case_when(FIESWholeDay > 1 ~ NA, T)) %>% 
  #Drop NAs from the FIES_ variables
  drop_na(FIESWorried, FIESHealthy, FIESSkipped, FIESRanOut, FIESWholeDay,
          FIESFewFoods, FIESHungry, FIESAteLess)

##Analyse data

fiesData %>% 
  #Group by dissaggregation variables
  group_by(Gender, Ethnicity, Language, IDPoor) %>% 
  #Find the sum of 1's "Yes"
  summarise(TotalWorried =  sum(FIESWorried),
            TotalHealthy = sum(FIESHealthy),
            TotalFewFoods = sum(FIESFewFoods),
            TotalSkipped = summ(FIESSkipped),
            TotalAteLess = sum(FIESAteLess),
            TotalRanOut = sum(FIESRanOut),
            TotalHungry = sum(FIESHungry),
            TotalWholeDay = sum(FIESWholeDay)) 
  
  

