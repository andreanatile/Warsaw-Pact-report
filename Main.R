library(readr)
library(tidyverse)
library(sqldf)
library(data.table)
library(ggplot2)
library(maps)
library(mapproj)
library(ggthemes)

#array of former soviet union country ----
soviet_countries <- c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia",
                      "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Moldova",
                      "Russian Federation", "Tajikistan", "Turkmenistan", "Ukraine", "Uzbekistan")
soviet_iso <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")
#array of former Warsaw pact country-----
warsaw_pact_country<-c(soviet_countries,"Romania","Czechia","Slovak Republic","Hungary","Poland","Bulgaria")
warsaw_pact_iso <- c("ALB", "BGR", "CSK", "DDR", "HUN", "POL", "ROU", soviet_iso)

#import dati gdp----
gdp_wb<-read_csv("gdp.csv") %>%
  pivot_longer(5:67,names_to="Year",values_to="gdp")%>%
  select(-4) %>%
  filter(Year>=1991) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)

#import gdp per capita data-----
gdp_xcapita_ppp<-read_csv("gdp_ppp.csv", skip = 4) %>%
  pivot_longer(5:67,names_to="Year",values_to="gdp_xcapita_ppp")%>%
  select(-4) %>%
  filter(Year>=1991) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)


#import data HDI----
HDI<-read.csv("HDR21-22_Statistical_Annex_HDI_Table.csv",skip=1,sep=";") %>%
  select(c(2,3)) %>%
  filter(Country %in% c(warsaw_pact_country,"Slovakia"))



#merge all the dataframe----

df_list<-list(gdp_wb,gdp_xcapita_ppp)
tot<-df_list %>%
  reduce(full_join,by=c("Country_Code","Country_Name","Year"))

#elminate NA value ------
tot[is.na(tot)]<-0
rm(gdp_wb,gdp_xcapita_ppp)




