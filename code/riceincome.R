library(tidyverse)


#This one to be calculated from the SAMS module. Need to to dis aggregate this between organic or non-organic rice income and organic rice income

data %>% 
  select(PSAMSRiceIncome) %>% 
  group_by(IDPoor, Gender, Ethnicity, Language) %>% 
  summarise(AverageRiceIncome = mean(PSAMSRiceIncome),
            MedianRiceIncome = median(PSAMSRiceIncome),
            TotalRiceIncome = sum(PSAMSRiceIncome)) %>% 
  ungroup()

#Draw a graph to show the comparison between organic rice income and non-organic rice income

ggplot()