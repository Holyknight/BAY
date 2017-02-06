#This plot is based on the TMB Analytics article:
#Understanding mortgage growth in Thailand: a regional perspective 
#link:http://www.bangkokpost.com/business/news/1191833/understanding-mortgage-growth-in-thailand-a-regional-perspective
#For any comment, please feel free to contact panawat.inn@tmbbank.com
#Thank you

library(data.table)
library(ggplot2)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(maptools)
library(dplyr)
#Set path
setwd("C:/Users/admin/Documents/3. R/Working Directory/MyStuff/THA_adm")
#Read Shapefile
map <- readRDS("THA_adm1.rds")
Name <- data.table(map$NAME_1)
names(Name) <- "NAME_1"

numMap<-length(map$NAME_1)
mydata<-data.frame(NAME_1=map$NAME_1, id=map$ID_1)
head(mydata)
#Read data
dss_temp <- read.csv("Mapping_province_dss_V3.csv", stringsAsFactors = F)

#Filter Data
#Measure
dss <- dss_temp %>% filter(Measure %in% cbind("Ticket_MB")) %>% 
  filter(Year %in% cbind('2016Q3', '2015', '2014', '2013'))

mydata2 <- mydata %>% left_join(dss, by = "NAME_1")

# impute data
mydata3 <- mydata2 %>% mutate(Value = ifelse(Measure == "Loan_Growth" & Value>20, 20,Value))

map_f <- fortify(map, region = "ID_1")

#merge with coefficients and reorder
merge_shp_coef<-merge(map_f, mydata3, by="id", all.x=TRUE)
final_plot<-merge_shp_coef[order(merge_shp_coef$order), ]
head(final_plot)

#Plot
g <- ggplot() +
  geom_polygon(data = final_plot, 
               aes(x = long, y = lat, group = group, fill = Value), 
               color = "white", 
               size = 0.25) + 
  coord_map()+
  scale_fill_distiller(name="Level", palette = "BuGn", breaks = pretty_breaks(n = 5), trans = "reverse", space = "Lab")+
  theme_nothing(legend = TRUE)+
  labs(title="Thailand's Mortgage Evolution") +
  facet_grid(Measure~Year, scales = "free") 
g
