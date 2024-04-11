library(tidyverse)


#Using the SAMS module. Need to to dis aggregate this between organic or non-organic rice income and organic rice income

data %>% 
  #Select the necessary variables for this analysis
  select(PSAMSRiceIncome, IDPoor, Gender, Ethnicity, Language, Organic) %>% 
  #Add grouping variables - Levels of dis aggregation
  group_by(IDPoor, Gender, Ethnicity, Language) %>% 
  #Calculate the necessary descriptive statistics
  summarise(AverageRiceIncome = mean(PSAMSRiceIncome),
            MedianRiceIncome = median(PSAMSRiceIncome),
            TotalRiceIncome = sum(PSAMSRiceIncome)) %>% 
  #Ungroup the variables - to enable further analysis
  ungroup()

#Draw a graph to show the comparison between organic rice income and non-organic rice income

ggplot()