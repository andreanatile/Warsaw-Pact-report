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
warsaw_pact_iso <- c("BGR", "CSK", "DDR", "HUN", "POL", "ROU", soviet_iso)



HDI<-read_csv("human-development-index.csv")%>%
  filter(Code %in% warsaw_pact_iso,Year==2021)%>%
  rename("HDI"="Human Development Index")
HDI1<-read_csv("human-development-index.csv")%>%
  filter(Code %in% c("ITA",warsaw_pact_iso),Year==2021)%>%
  rename("HDI"="Human Development Index")


#plot a map of gdp x capita of 2021----
world <- ne_countries(scale = "medium", returnclass = "sf")

merged_data<-merge(world,HDI,by.x="sov_a3",by.y="Code")

map_HDI<-merged_data %>%ggplot() + 
  geom_sf(color = "black", aes(fill = HDI))+
  scale_fill_gradient(name="HDI by country 2021",low="lightblue1",high="blue4")+
  theme(panel.border = element_blank(), axis.line = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  coord_sf(xlim = c(10, 90))
plot(map_HDI)
#gdp bar----
bar_HDI <-ggplot(HDI1, aes(x = HDI, y = reorder(Entity,HDI),fill=HDI)) +
  geom_bar(stat = "identity",orientation="y") +
  xlab("") +
  ylab("") +
  theme(panel.border = element_blank(), axis.line = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")+
  scale_fill_gradient(low="lightblue1",high="blue4")

plot(bar_HDI)

plot_grid(bar_HDI, map_HDI, align = "v", axis = "tb", ncol = 2, rel_widths = c(1, 1.5))

