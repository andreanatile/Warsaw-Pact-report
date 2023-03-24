library(readr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(maps)
library(mapproj)
library(ggthemes)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(cowplot)

#array of former soviet union country ----
soviet_countries <- c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia",
                      "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Moldova",
                      "Russian Federation", "Tajikistan", "Ukraine", "Uzbekistan")
soviet_iso <- c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB")
#array of former Warsaw pact country-----
warsaw_pact_country<-c(soviet_countries,"Romania","Czechia","Slovak Republic","Hungary","Poland","Bulgaria")
warsaw_pact_iso <- c("ALB", "BGR", "CSK", "DDR", "HUN", "POL", "ROU", soviet_iso)

#import dati gdp----
gdp_wb<-read_csv("gdp.csv") %>%
  pivot_longer(5:67,names_to="Year",values_to="gdp")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)

#import gdp per capita data-----
gdp_xcapita_ppp<-read_csv("gdp_ppp.csv", skip = 4) %>%
  pivot_longer(5:67,names_to="Year",values_to="gdp_xcapita_ppp")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)
#import gdp growth data------
gdp_growth<-read_csv("gdp_growth.csv",skip = 4 )%>%
  pivot_longer(5:67,names_to="Year",values_to="gdp_growth")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)

#import data population_growth----
population_growth<-read_csv("population_growth.csv",skip = 4 )%>%
  pivot_longer(5:67,names_to="Year",values_to="population_growth")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)


#import data net migration----
net_migration<-read_csv("net_migration.csv",skip = 4 )%>%
  pivot_longer(5:67,names_to="Year",values_to="net_migration")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)


#import data life expectancy----
life_expectancy<-read_csv("Life_expectancy.csv",skip = 4 )%>%
  pivot_longer(5:67,names_to="Year",values_to="life_expectancy")%>%
  select(-4) %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)
#import foreign investment net flow----
foreign_investement<-read_csv("foreign_investment_net_flow.csv",skip=4) %>%
  pivot_longer(5:67,names_to="Year",values_to="investment_net_flow")%>%
  select(-4) %>%
  rename("Country_Name"="Country Name","Country_Code"="Country Code") %>%
  filter(Year>=1990) %>%
  filter(Country_Name %in% c("European Union",warsaw_pact_country)) %>%
  select(-3)
#merge all the dataframe----

df_list<-list(gdp_growth,gdp_wb,gdp_xcapita_ppp,life_expectancy,net_migration,population_growth,foreign_investement)
tot<-df_list %>%
  reduce(full_join,by=c("Country_Code","Country_Name","Year"))

#elminate NA value ------
tot[is.na(tot)]<-0
rm(gdp_wb,gdp_growth,gdp_xcapita_ppp,foreign_investement,net_migration,life_expectancy,df_list,population_growth)


#gdp_x_capita 2021----
gdp_xcapita_ppp_2021<-subset(tot,Year==2021,
                             select=c("Country_Name","Country_Code","gdp_xcapita_ppp"))
gdp_xcapita_ppp_2021<- gdp_xcapita_ppp_2021[order(gdp_xcapita_ppp_2021$gdp_xcapita_ppp,decreasing = TRUE)]

#plot a map of gdp x capita of 2021----
world <- ne_countries(scale = "medium", returnclass = "sf")

merged_data<-merge(world,gdp_xcapita_ppp_2021,by.x="sov_a3",by.y="Country_Code")

map_gdp_xcapita<-merged_data %>%ggplot() + 
  geom_sf(color = "black", aes(fill = gdp_xcapita_ppp))+
  scale_fill_gradient(name="GDP(PPP) per capita USD 2021",low="red",high="green")+
  theme(panel.border = element_blank(), axis.line = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  coord_sf(xlim = c(10, 90))
plot(map_gdp_xcapita)
#gdp bar----
bar_gdp_xcapita <-ggplot(gdp_xcapita_ppp_2021, aes(x = gdp_xcapita_ppp, y = reorder(Country_Name,gdp_xcapita_ppp),fill=gdp_xcapita_ppp)) +
  geom_bar(stat = "identity",orientation="y") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_blank(), axis.line = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")+
  scale_fill_gradient(low="red",high="green")
  
plot(bar_gdp_xcapita)

plot_grid(bar_gdp_xcapita, map_gdp_xcapita, align = "v", axis = "tb", ncol = 2, rel_widths = c(1, 1.5))

