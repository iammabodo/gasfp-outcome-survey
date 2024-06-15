# Loading required packages
library(tidyverse)
library(gt)
library(readxl)


# Reading the data

HHDisability <- read_excel("data/Copy of Data_Format_WFP_GASFP_WO8.xlsx",
                           sheet = "Roster_HHList") %>% 
  select(interview__key, contains("Disab"))