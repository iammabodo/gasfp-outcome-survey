library(tidyverse)

library(tidyverse)
library(labelled)
library(expss)

#Insert correct data code here
#data <- read_csv("~/GitHub/RAMResourcesScripts/Static/Nut_MAD_Sample_Survey/MAD_submodule_RepeatMAD.csv")

#Rename variables to suit the project

data <- data %>% rename(PCMADChildAge_months = 'MAD_submodule/RepeatMAD/PCMADChildAge_months',
                        PCMADBreastfeed = 'MAD_submodule/RepeatMAD/PCMADBreastfeed',
                        PCMADInfFormula = 'MAD_submodule/RepeatMAD/PCMADInfFormula',
                        PCMADInfFormulaNum = 'MAD_submodule/RepeatMAD/PCMADInfFormulaNum',
                        PCMADMilk = 'MAD_submodule/RepeatMAD/PCMADMilk',
                        PCMADMilkNum = 'MAD_submodule/RepeatMAD/PCMADMilkNum',
                        PCMADYogurtDrink = 'MAD_submodule/RepeatMAD/PCMADYogurtDrink',
                        PCMADYogurtDrinkNum = 'MAD_submodule/RepeatMAD/PCMADYogurtDrinkNum',
                        PCMADYogurt = 'MAD_submodule/RepeatMAD/PCMADYogurt',
                        PCMADStapCer = 'MAD_submodule/RepeatMAD/PCMADStapCer',
                        PCMADVegOrg = 'MAD_submodule/RepeatMAD/PCMADVegOrg',
                        PCMADStapRoo = 'MAD_submodule/RepeatMAD/PCMADStapRoo',
                        PCMADVegGre = 'MAD_submodule/RepeatMAD/PCMADVegGre',
                        PCMADVegOth = 'MAD_submodule/RepeatMAD/PCMADVegOth',
                        PCMADFruitOrg = 'MAD_submodule/RepeatMAD/PCMADFruitOrg',
                        PCMADFruitOth = 'MAD_submodule/RepeatMAD/PCMADFruitOth',
                        PCMADPrMeatO = 'MAD_submodule/RepeatMAD/PCMADPrMeatO',
                        PCMADPrMeatPro = 'MAD_submodule/RepeatMAD/PCMADPrMeatPro',
                        PCMADPrMeatF = 'MAD_submodule/RepeatMAD/PCMADPrMeatF', 
                        PCMADPrEgg = 'MAD_submodule/RepeatMAD/PCMADPrEgg',
                        PCMADPrFish = 'MAD_submodule/RepeatMAD/PCMADPrFish',
                        PCMADPulse = 'MAD_submodule/RepeatMAD/PCMADPulse',
                        PCMADCheese = 'MAD_submodule/RepeatMAD/PCMADCheese',
                        PCMADSnf = 'MAD_submodule/RepeatMAD/PCMADSnf',
                        PCMADMeals = 'MAD_submodule/RepeatMAD/PCMADMeals'
)
