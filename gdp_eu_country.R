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

#make different subset of country----
join_europe_country<-c("Romania","Czechia","Slovak Republic","Hungary","Poland","Bulgaria","Estonia", "Latvia", "Lithuania")
join_europe_country1<-c("Czechia","Hungary","Poland") #NATO 1999 EU 2004
join_europe_country2<-c("Estonia", "Latvia", "Lithuania","Slovak Republic") #NATO and EU 2004
join_europe_country3<-c("Romania","Bulgaria") # NATO 2004 EU 2007
no_join<-c("Armenia", "Azerbaijan", "Belarus","Kazakhstan", "Kyrgyzstan", "Tajikistan",  "Uzbekistan","Georgia")
uka_russ<-c("Russian Federation","Ukraine")

no_join_gdp<- subset(tot, Country_Name %in% no_join , select = c("Country_Name","Country_Code","Year", "gdp"))
no_join_gdp$gdp<-no_join_gdp$gdp/10^9

uka_gdp<- subset(tot, Country_Name=="Ukraine" , select = c("Country_Name","Country_Code","Year", "gdp"))
uka_gdp$gdp<-uka_gdp$gdp/10^9

russ_gdp<- subset(tot, Country_Name=="Russian Federation" , select = c("Country_Name","Country_Code","Year", "gdp"))
russ_gdp$gdp<-russ_gdp$gdp/10^9

gdp_eu_country1 <- subset(tot, Country_Name %in% join_europe_country1 , select = c("Country_Name","Country_Code","Year", "gdp"))
gdp_eu_country1$gdp<-gdp_eu_country1$gdp/10^9

gdp_eu_country2<- subset(tot, Country_Name %in% join_europe_country2, select = c("Country_Name","Country_Code","Year", "gdp"))
gdp_eu_country2$gdp<-gdp_eu_country2$gdp/10^9

gdp_eu_country3 <- subset(tot, Country_Name %in% join_europe_country3 , select = c("Country_Name","Country_Code","Year", "gdp"))
gdp_eu_country3$gdp<-gdp_eu_country3$gdp/10^9


# map country joining NATO in 1999 and EU 2004----
chart_gdp1<-ggplot(data=gdp_eu_country1,aes(x=Year,y=gdp,group=Country_Name,color=Country_Name))+
  geom_line(stat="identity",size=0.8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2008", linetype = "dashed") +
  geom_vline(xintercept = "1999", linetype = "dashed")+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  ggtitle("GDP by Country joining NATO in 1999 and EU in 2004") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=18.6, y=400, label="Globa recession", angle=90, size=3, color="grey50")+
  annotate("text", x=14.6, y=400, label="Join EU", angle=90, size=3, color="grey50")+
  annotate("text", x=9.6, y=400, label="Join NATO", angle=90, size=3, color="gray50")



#chart of gdp of country join NATO and EU in 2004
chart_gdp2<-ggplot(data=gdp_eu_country2,aes(x=Year,y=gdp,group=Country_Name,color=Country_Name))+
  geom_line(stat="identity",size=0.8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2008", linetype = "dashed") +
  geom_vline(xintercept = "2004", linetype = "dashed")+
  ggtitle("GDP by Country joining NATO and UE in 2004") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=18.6, y=70, label="Globa recession", angle=90, size=3, color="grey50")+
  annotate("text", x=14.6, y=80, label="Join NATO and EU", angle=90, size=3, color="grey50")

#chart of gdp country join NATO in 2004 and EU in 2007----
chart_gdp3<-ggplot(data=gdp_eu_country3,aes(x=Year,y=gdp,group=Country_Name,color=Country_Name))+
  geom_line(stat="identity",size=0.8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2008", linetype = "dashed") +
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2007", linetype = "dashed")+
  ggtitle("GDP by Country joining NATO in 2004 and EU in 2007") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=18.6, y=120, label="Globa recession", angle=90, size=3, color="grey50")+
  annotate("text", x=17.6, y=120, label="Join EU", angle=90, size=3, color="grey50")+
  annotate("text", x=14.6, y=120, label="join NATO", angle=90, size=3, color="gray50")
plot(chart_gdp3)

chart_gdp4<-ggplot(data=no_join_gdp,aes(x=Year,y=gdp,group=Country_Name,color=Country_Name))+
  geom_line(stat="identity",size=0.8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2008", linetype = "dashed") +
  ggtitle("GDP by remaining Country") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=18.6, y=120, label="Globa recession", angle=90, size=3, color="grey50")

chart_join_eu_and_not<-plot_grid(chart_gdp1,chart_gdp2,chart_gdp3,chart_gdp4,align = "hv",nrow = 2,ncol = 2)


chart_gdp5<-ggplot(data=uka_gdp,aes(x=Year,y=gdp))+
  geom_line(stat="identity",size=0.8,group=1,color="green")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2014", linetype = "dashed") +
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2008", linetype = "dashed")+
  ggtitle("GDP of Ukraine by Year") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=24.6, y=120, label="Annexation of Crimea", angle=90, size=3, color="grey50")+
  annotate("text", x=18.6, y=120, label="Global Recession", angle=90, size=3, color="grey50")+
  annotate("text", x=14.6, y=120, label="Orange Revolution", angle=90, size=3, color="gray50")

chart_gdp6<-ggplot(data=russ_gdp,aes(x=Year,y=gdp))+
  geom_line(sta="identity",size=0.8,color="blue",group=1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA))+
  scale_x_discrete(name = "Year", breaks = seq(1991, 2021, by = 3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = "2014", linetype = "dashed") +
  geom_vline(xintercept = "2008", linetype = "dashed")+
  geom_vline(xintercept = "2000", linetype = "dashed")+
  ggtitle("GDP of Russia by Year") +
  ylab("GDP (billions USD)") +
  xlab("Year")+
  labs(color = "Country")+
  annotate("text", x=18.6, y=1200, label="Globa recession", angle=90, size=3, color="grey50")+
  annotate("text", x=24.6, y=1200, label="Annexation of Crimea", angle=90, size=3, color="grey50")+
  annotate("text", x=10.6, y=1200, label="Chechen war", angle=90, size=3, color="gray50")
plot(chart_gdp6)
chart_uka_russia<-plot_grid(chart_gdp5,chart_gdp6,align = "v")

plot(chart_uka_russia)
plot(chart_join_eu_and_not)
plot_grid(chart_gdp1,chart_gdp2,align="v")
plot_grid(chart_gdp3,chart_gdp4)
