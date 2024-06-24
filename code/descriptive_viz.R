library(tidyverse)
library(ggtext)



# Load the data

HHData <- read_excel("data/FullHHRosterClean.xlsx") %>% 
  # Select the necessary columns
  select(interview_key, HHID, HHList, 
         HHBaseline, HHHEthnicity, HHHLanguage, 
         IDPoor, HHHSex, ACName, ADMIN4Name, NumDisab, HHDisab) %>% 
  distinct(interview_key, .keep_all = TRUE) %>% 
  # Change the Character Variables to Factors
  mutate(HHBaseline = as.factor(HHBaseline),
         HHHEthnicity = as.factor(HHHEthnicity),
         HHHLanguage = as.factor(HHHLanguage),
         IDPoor = as.factor(IDPoor),
         HHHSex = as.factor(HHHSex),
         ACName = as.factor(ACName),
         ADMIN4Name = as.factor(ADMIN4Name),
         NumDisab = as.factor(NumDisab),
         HHDisab = as.factor(HHDisab))


VizData <- HHData %>% 
  group_by(ACName, HHBaseline, HHHSex) %>%
  summarise(n = n())

# Subtitle text
subtitle_text <- "Number of households <span style='color:#00BFC4'>**Male**</span> headed and 
<span style='color:#F8766D'>**Female**</span> headed"

VizData %>% 
  ggplot(aes(x = ACName, 
             y = n, 
             fill = HHHSex)) +
  geom_point(position = position_jitterdodge(seed = 123), 
             size = 4,
             alpha = 0.75,
             shape = 21) + 
  theme_minimal(base_size = 14,
                base_family = "Times New Roman") + # Should Learn How to use different fonts
  theme(plot.subtitle = ggtext::element_markdown(),
        legend.position = "none") + 
  labs(title = "Number of Households by AC Name",
       x = "Agriculture Cooperative Name",
       y = "Number of Households",
       fill = "Gender",
       subtitle = subtitle_text)
  
