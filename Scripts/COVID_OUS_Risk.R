# COVID-19 Risk Assessment
#
# Assess risk for non-US countries by country based on active cases and if the county is in lockdown or not.
#
# Includes a webscraper to pull tabular data from worldometers website.
# Active cases of COVID-19 is combined with an updated list (at a given date) of countries in lockdown.
# Based on these factors each country is given a risk number and related discount value and multiplier which is output
#
#


library(dplyr)
library(rvest)

#Set write path
root <- "/lrlhps/users/c269016/covid19/"
filenm <- paste0("active", Sys.Date(), ".csv")

#Country level data (OUS & US)
url <- "https://www.worldometers.info/coronavirus/"
OUS <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
  html_table()
OUS <- OUS[[1]]

# write.csv(OUS, paste0(root, filenm), row.names = F)

#US state level data
url <- "https://www.worldometers.info/coronavirus/country/us/"
states <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='//*[@id="usa_table_countries_today"]') %>%
  html_table(fill=TRUE)
states <- states[[1]] %>% dplyr::select(-Source)

# write.csv(states, paste0(root, "states", filenm), row.names = F)


world_corona <- OUS[-c(1:8), ]
colnames(world_corona)[1] <- "Country.Other"

#  Define if country is in lockdown
final_set <- world_corona
  final_set$Lockdown <- "No"
  final_set$Lockdown <- ifelse (final_set$Country.Other=="Australia"| final_set$Country.Other=="Belgium"| final_set$Country.Other=="France"| final_set$Country.Other=="Italy"| 
                                 final_set$Country.Other=="Spain"| final_set$Country.Other=="Argentina"
                                | final_set$Country.Other=="Colombia"| final_set$Country.Other=="Czechia"| final_set$Country.Other=="Denmark"
                                | final_set$Country.Other=="El Salvador"| final_set$Country.Other=="Germany"| final_set$Country.Other=="India"
                                | final_set$Country.Other=="Israel"| final_set$Country.Other=="Malaysia"| final_set$Country.Other=="Morocco"
                                | final_set$Country.Other=="Kuwait"| final_set$Country.Other=="Jordan"| final_set$Country.Other=="Kenya"
                                | final_set$Country.Other=="New Zealand"| final_set$Country.Other=="Norway"| final_set$Country.Other=="Poland"
                                | final_set$Country.Other=="Russia"| final_set$Country.Other=="Saudi Arabia"| final_set$Country.Other=="Ireland"
                                | final_set$Country.Other=="South Africa"| final_set$Country.Other=="UK"| final_set$Country.Other=="USA", "Yes", "No")
  final_set$ActiveCases <- as.numeric(gsub(",","",final_set$ActiveCases))

  # Separate countries by number of active cases
  final_set <- final_set %>% mutate(Risk_Number = case_when((ActiveCases < 1000) ~ 1,
                                                            (ActiveCases >999 & ActiveCases < 5000) ~ 2,
                                                            (ActiveCases >4999 & ActiveCases < 10000) ~ 3,
                                                            (ActiveCases >9999 & ActiveCases < 20000) ~ 4,
                                                            (ActiveCases >=20000) ~ 5
  ))
  # Set Risk value and discount value/multiplier
  final_set$Risk_Number <- final_set$Risk_Number + ifelse(final_set$Lockdown=="Yes",1,0)
  final_set <- final_set %>% mutate(Risk = case_when(Risk_Number==1 ~ "Very Low Risk",
                                                     Risk_Number==2 ~ "Low Risk",
                                                     Risk_Number==3 ~ "Medium Risk",
                                                     Risk_Number==4 ~ "High Risk",
                                                     Risk_Number==5 ~ "Very High Risk",
                                                     Risk_Number==6 ~ "Very High Risk"))
  
  final_set <- final_set %>% mutate(Discount = case_when(Risk_Number==1 ~ 0,
                                                     Risk_Number==2 ~ 0.2,
                                                     Risk_Number==3 ~ 0.5,
                                                     Risk_Number==4 ~ 0.8,
                                                     Risk_Number==5 ~ 1,
                                                     Risk_Number==6 ~ 1))
  
  final_set <- final_set %>% mutate(Multiplier = case_when(Risk_Number==1 ~ 1,
                                                         Risk_Number==2 ~ 0.8,
                                                         Risk_Number==3 ~ 0.5,
                                                         Risk_Number==4 ~ 0.2,
                                                         Risk_Number==5 ~ 0,
                                                         Risk_Number==6 ~ 0))
  

final_set <- final_set[c("Country.Other","ActiveCases","Lockdown","Risk","Discount","Multiplier")]
  

write.csv(final_set, "OUS_risk_April20.csv")





