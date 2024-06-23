library(tidyverse)

# Reading the data.  
HHDisabRoster <- read_excel("data/HHRoster_Cleaned_Numeric.xlsx") %>% 
  select(interview_key, HHRoster_id, HHFulName, HHMemsex, HHMemAge, contains("Disab")) %>% 
  mutate(PDisabSee = case_when(
    PDisabSee == 1 ~ "No difficulty",
    PDisabSee == 2 ~ "Some difficulty",
    PDisabSee == 3 ~ "A lot of difficulty",
    PDisabSee == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  DisabHear = case_when(
    DisabHear == 1 ~ "No difficulty",
    DisabHear == 2 ~ "Some difficulty",
    DisabHear == 3 ~ "A lot of difficulty",
    DisabHear == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabWalk = case_when(
    PDisabWalk == 1 ~ "No difficulty",
    PDisabWalk == 2 ~ "Some difficulty",
    PDisabWalk == 3 ~ "A lot of difficulty",
    PDisabWalk == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabRemember = case_when(
    PDisabRemember == 1 ~ "No difficulty",
    PDisabRemember == 2 ~ "Some difficulty",
    PDisabRemember == 3 ~ "A lot of difficulty",
    PDisabRemember == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabUnderstand = case_when(
    PDisabUnderstand == 1 ~ "No difficulty",
    PDisabUnderstand == 2 ~ "Some difficulty",
    PDisabUnderstand == 3 ~ "A lot of difficulty",
    PDisabUnderstand == 4 ~ "Cannot do at all",
    TRUE ~ NA
  ),
  PDisabWash = case_when(
    PDisabWash == 1 ~ "No difficulty",
    PDisabWash == 2 ~ "Some difficulty",
    PDisabWash == 3 ~ "A lot of difficulty",
    PDisabWash == 4 ~ "Cannot do at all",
    TRUE ~ NA)) %>% 
# Mutate a variable to show that a member is disabled if they have a disability in any of the 6 categories
  mutate(Disab = case_when(
    PDisabSee == "Some difficulty" | PDisabSee == "A lot of difficulty" | PDisabSee == "Cannot do at all" |
      DisabHear == "Some difficulty" | DisabHear == "A lot of difficulty" | DisabHear == "Cannot do at all" |
      PDisabWalk == "Some difficulty" | PDisabWalk == "A lot of difficulty" | PDisabWalk == "Cannot do at all" |
      PDisabRemember == "Some difficulty" | PDisabRemember == "A lot of difficulty" | PDisabRemember == "Cannot do at all" |
      PDisabUnderstand == "Some difficulty" | PDisabUnderstand == "A lot of difficulty" | PDisabUnderstand == "Cannot do at all" |
      PDisabWash == "Some difficulty" | PDisabWash == "A lot of difficulty" | PDisabWash == "Cannot do at all" ~ "Yes",
    TRUE ~ "No")) %>% 
# Mutate HHMemsex to be equal to Female if 0 or Male if 1
  mutate(HHMemsex = case_when(
  HHMemsex == 0 ~ "Female",
  HHMemsex == 1 ~ "Male")) %>%
  # Group by the household ID and count the number of disabled members
  group_by(interview_key) %>%
  summarise(NumDisab = sum(Disab == "Yes", na.rm = TRUE)) %>%
  # Mutate a variable to show if the household has a disabled member
  mutate(HHDisab = case_when(
    NumDisab > 0 ~ "Yes",
    TRUE ~ "No"))

# Calculate the percentage of households with disabled members
HHDisabRoster %>% 
  count(HHDisab) %>% 
  mutate(Percentage = round(100 * n / sum(n), 2))
  
  
  
  
  
  
