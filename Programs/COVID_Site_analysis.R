# COVID-19 Risk Assessment
#
# Function that combines study site information to data on the spread COVID-19 to assess risk to a given study.
#
# Links proprietary study site data with spot date COVID_19 data from the US and globally through 
# zipcode data (US only) pulled from https://www.unitedstateszipcodes.org/zip-code-database/
# 
# Function takes in study site data as an input.
#
# Results include which study sites (both US and globally) may have risk due to COVID-19 spread associated with them.
# Outputs site information, total number of sites, and level of risk.
#


suppressMessages(library(tidyverse))


#site_data <- read.csv("site_data.csv") ### Proprietary data, layout given below
# SITE	PatientCount	CompleteIn90Days	CompleteNow	STREET	STATE_PROVINCE	CITY	ZIPCODE	COUNTRYCD	COUNTRY

location <- read.csv("Zip_data.csv")
corona_data <- read.csv("Corona_data.csv")
world_corona <- read.csv("world_data_corona.csv")


Risk_assessment <- function (study) {
  
  site_data <<- read.csv(study)
  

  world_corona <- rename(world_corona, Death = Deaths)
  world_corona$Confirmed<-as.integer(world_corona$Confirmed)
  world_corona$Death<-as.integer(world_corona$Death)
  world_corona$Recovered<-as.integer(world_corona$Recovered)
  world_corona <- world_corona[c("Confirmed","Death","Recovered","Country.Region")]
  asdf <- world_corona %>% group_by(Country.Region) %>% summarise_all(list)
  
  world_corona <- world_corona %>% group_by(Country.Region) %>% summarise(Confirmed = sum(Confirmed),Death=sum(Death),Recovered=sum(Recovered))
  world_corona <- data.frame(apply(world_corona,2, toupper))
  world_corona <- rename(world_corona, COUNTRY = Country.Region)
  
  
  site_data <- rename(site_data, State_abb = STATE_PROVINCE)
  site_data <- rename(site_data, zip = ZIPCODE)
  
  corona_data <- rename(corona_data, county= County_Name)
  corona_data$State_abb<- state.abb[match(corona_data$State_Name, state.name)]
  
  
  no_us_site <- site_data %>% filter(COUNTRYCD != "US")
  
  worldmerge <- merge(no_us_site, world_corona, by= "COUNTRY")
  
  zipmerge <- merge(site_data, location, by ="zip")
  zipmerge$county <-gsub(" County","",zipmerge$county) 
  zipmerge <- zipmerge %>% filter(COUNTRYCD=="US")
  
  threemerge <- merge(zipmerge, corona_data, by=c("county","State_abb"))
  allmerge <- merge(threemerge, worldmerge, all=TRUE)
  allmerge<-allmerge[order(allmerge$COUNTRYCD,decreasing=TRUE),]
  all_cases <- allmerge[c("COUNTRY","COUNTRYCD","State_Name","State_abb","zip","county","CITY","SITE","PatientCount","CompleteIn90Days","CompleteNow","Confirmed","Death","New","Last_Update")]
  
  nomatch <- anti_join(site_data,allmerge, by="SITE")
  nomatch$State_Name<- state.name[match(nomatch$State_abb, state.abb)]
  
  nomatch$Death = 0
  nomatch$New = 0
  nomatch$Confirmed = 0
  nomatch$Last_Update = NA
  nomatch$county = NA
  world_nous <- nomatch[c("COUNTRY","COUNTRYCD", "State_Name","State_abb","zip","county","CITY","SITE","PatientCount","CompleteIn90Days","CompleteNow","Confirmed","Death","New","Last_Update")]

  final_set <- rbind(all_cases, world_nous)
  final_set$Lockdown <- "No"
  final_set$Lockdown <- ifelse(final_set$State_abb=="NY" | final_set$State_abb=="CA"| final_set$State_abb== "IL"| final_set$State_abb== "PA"| final_set$State_abb=="NJ"| final_set$State_abb=="WA"| final_set$State_abb=="CT"| 
           final_set$State_abb=="LA"| final_set$State_abb=="MN" | final_set$COUNTRY=="BELGIUM"| final_set$COUNTRY=="FRANCE"| final_set$COUNTRY=="ITALY"| 
            final_set$COUNTRY=="SPAIN", "Yes", "No")
  final_set$Confirmed <- as.integer(final_set$Confirmed)
  
  final_set <- final_set %>% mutate(Risk_Number = case_when((Confirmed < 1000 & COUNTRYCD != "US")|(Confirmed <10 ) ~ 1,
                                                      (Confirmed >999 & Confirmed < 5000 &  COUNTRYCD != "US") | (Confirmed <100 & Confirmed>9 &  COUNTRYCD == "US") ~ 2,
                                                      (Confirmed >4999 & Confirmed < 10000 &  COUNTRYCD != "US") | (Confirmed <500 & Confirmed>99 &  COUNTRYCD == "US") ~ 3,
                                                      (Confirmed >9999 & Confirmed < 20000 &  COUNTRYCD != "US") | (Confirmed <1000 & Confirmed>499 &  COUNTRYCD == "US") ~ 4,
                                                      (Confirmed >=20000 &  COUNTRYCD != "US") | (Confirmed >=1000 &  COUNTRYCD == "US") ~ 5
                                    ))
  
  final_set$Risk_Number <- final_set$Risk_Number + ifelse(final_set$Lockdown=="Yes",1,0)
  final_set <- final_set %>% mutate(Risk = case_when(Risk_Number==1 ~ "Very Low Risk",
                                                     Risk_Number==2 ~ "Low Risk",
                                                     Risk_Number==3 ~ "Medium Risk",
                                                     Risk_Number==4 ~ "High Risk",
                                                     Risk_Number==5 ~ "Very High Risk",
                                                     Risk_Number==6 ~ "Very High Risk"))
  
  final_set$CompleteIn90Days[is.na(final_set$CompleteIn90Days)] <-0
  final_set$CompleteNow[is.na(final_set$CompleteNow)] <-0
  
  sum_patients <- final_set %>% group_by(Risk) %>% summarize(Sites =n(), Total_Patients = sum(PatientCount), Complete_Now = sum(CompleteNow), Complete90Days_Ori = sum(CompleteIn90Days))
  df <- data.frame("Risk"= c("High Risk","Very High Risk","Medium Risk","Low Risk","Very Low Risk"), "Discount_Factor"= c(0.8,1,0.5,0.1,0))
  sum_patients <- merge(sum_patients,df,all=TRUE)
  sum_patients$After_Discount90Days <- (1-sum_patients$Discount_Factor) * sum_patients$Complete90Days
  sum_patients<-sum_patients[order(sum_patients$Discount_Factor,decreasing=TRUE),]
  sum_patients[is.na(sum_patients)] <- 0
  sum_patients[6,] = c("Overall", colSums(sum_patients[,2:7]))
  
  final_set <<- final_set
  sum_patients <<- sum_patients
  #check <- final_set %>% filter(Risk=="Very Low Risk"& COUNTRY=="UNITED STATES")
  
  world_final <- final_set %>% filter(COUNTRYCD !="US")
  world_sumrisk <- world_final %>% group_by(Risk) %>% summarize(total=n())
  us_final <- final_set %>% filter(COUNTRYCD == "US")
  us_sumrisk <- us_final %>% group_by(Risk) %>% summarize (total=n())

}


Risk_assessment("site_data.csv")
Risk_assessment("GPHK_SITE_INFO.csv")
Risk_assessment("GPGH_SITE_INFO.csv")
Risk_assessment("GPGM_SITE_INFO.csv")
Risk_assessment("GPGL_SITE_INFO.csv")
Risk_assessment("GPGI_SITE_INFO.csv")



write.csv(allmerge, "GBGK Site Match to Coronavirus.csv")





