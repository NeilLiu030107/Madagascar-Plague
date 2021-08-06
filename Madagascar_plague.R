#Loading libraries
library(tidyverse)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(lubridate)

###Part 1: Read data

#Read cases by regions and hospitals from 10/20/2017 to 11/13/2017
plague <- read.csv("Data/Disease_data.csv")
View(plague)

#Read population data
population <- read.csv("Data/Population.csv")

#Adding prevalence and mortality columns to plague.csv
plague <-inner_join(plague, population, by="Name")

plague <-mutate(plague, Prevalence = plague$Undertreatment / plague$Population*1000, Mortality=plague$cumulative.death/(plague$cumulative.healed+plague$cumulative.death))

#Read geographic info of main cities(temp, elevation, coordination, precipitation)
geographic <- read.csv("https://raw.githubusercontent.com/systemsmedicine/plague2017/master/data/1180122.csv")

View(geographic)

#Population density
pop_density <- raster("Data/mdg_pd_2017_1km_UNadj.tif")


#Night light
night_light <- raster("Data/mdg_viirs_100m_2016.tif")
night_light_resampled <- resample(night_light, pop_density, method="ngb")

#Elevation
elev <- raster::getData(name="alt", country="MDG")

#Travel time to healthcare facilities
mdg_travel_time <- raster("Data/2020_walking_only_travel_time_to_healthcare.geotiff")

mdg_adm_0 <- raster::getData("GADM", country="MDG", level=0) 

dist_mdg <- raster::crop(x = mdg_travel_time, y = mdg_adm_0)

dist_mdg_mask <- raster::mask(x = dist_mdg, mask = mdg_adm_0)

#Check spatial data frames
pop_density
night_light
elev
dist_mdg_mask

#leaflet image for population density

pal <- colorNumeric("viridis", values(pop_density), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(pop_density, colors = pal, opacity = 0.8) %>%
  addLegend(pal=pal, values = values(pop_density), title="Population density per km^2")


#leaflet image for night light

pal2 <- colorNumeric("viridis", domain=c(0,50), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(night_light_resampled, colors = pal2, opacity = 0.8) %>%
  addLegend(pal=pal2, values = c(0,50), title="Night light")

#leaflet image for elevation
pal3 <- colorNumeric("viridis", values(elev), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(elev, colors = pal3, opacity = 0.8) %>%
  addLegend(pal=pal3, values = values(elev), title="Elevation(m)")

#leaflet image for travel time to health facilities
pal4 <- colorNumeric("viridis", values(dist_mdg_mask), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(dist_mdg_mask, colors = pal4, opacity = 0.8) %>%
  addLegend(pal=pal4, values = values(dist_mdg_mask), title="Travel time to health facilities(min)")
 

 
###Part 2: Mapping regional pattern of total plague cases in Madagascar

# 10/20/2017 cases
total_10202017 <- plague %>%
  subset(Date=="10/20/2017"& Type=="Region") %>%
  select(Name, Total, Prevalence, Mortality)
head(total_10202017)

#11/13/2017 cases
total_11132017 <- plague %>%
  subset(Date=="11/13/2017"& Type=="Region") %>%
  select(Name, Total, Prevalence, Mortality)
head(total_10202017)

#Madagascar Provincial Map
mdg_adm_2 <- raster::getData("GADM", country="MDG", level=2)
names(mdg_adm_2)

##Mapping function
Choropleths <-function(data){
  #Join attributes
  Map <- sp::merge(mdg_adm_2, data, by.x="NAME_2", by.y="Name", all.x=TRUE)
  
  Map@data$Total[which(is.na(Map@data$Total))]<- 0
  
  #color
  bins <-c(0,5,10,20,50,100,200,400,Inf)
  pal <- colorBin("YlOrRd", domain = Map$Total, bins = bins)
  
  #label
  labels <- sprintf(
    "<strong>%s</strong><br/>%g case(s)",
    Map$NAME_2, Map$Total
  ) %>% lapply(htmltools::HTML)
  
  #mapping
  leaflet(Map) %>%
    setView(46,-18,4) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(Total),
      weight = 2,
      color="white",
      dashArray = "3",
      opacity=1,
      fillOpacity = 0.7,
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 16px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~Total, opacity = 0.7, title = "Total cases")
  
}

#10/20/2017 Total cases map (mid-outbreak)
Choropleths(total_10202017)

#11/13/2017 Total cases map (end-outbreak)
Choropleths(total_11132017)

#Most plague cases were distributed in the central area and central east coast of Madagascar, particularly the Analamanga region, where the country's capital (also the largest city) located. It seemed the transportation around the capital promoted the rapid spread of plague in the nearby regions. Other provinces in the west coast also reported plague cases but with much fewer numbers and the spread was much slower. 

#Another thing that comes to my notice is that the reports from Madagascar government(used in this research) recorded lower cases than the WHO reports did. Therefore, the data may underestimate the real situation in Madagascar.



###Part 3: Spatial analysis of the correlation between covariants and prevalence & mortality

#Extract data to spatial polygon data frame that represents the admistrative zones of Madagascar
mdg_adm_2$pop_density <- raster::extract(pop_density,mdg_adm_2, fun=mean, na.rm=TRUE)

mdg_adm_2$night_light <- raster::extract(night_light_resampled, mdg_adm_2, fun=mean, na.rm= TRUE)

mdg_adm_2$travel_time <- raster::extract(dist_mdg_mask, mdg_adm_2, fun=mean, na.rm=TRUE)

mdg_adm_2$elevation <- raster::extract(elev, mdg_adm_2, fun=mean, na.rm=TRUE)

mdg_adm_2 <- merge(mdg_adm_2, population, by.x="NAME_2", by.y="Name", all.x=TRUE)

View(mdg_adm_2@data)
  
##Plotting temperature & precipitation
#Transform geographic dataframe into spatial point dataframe
geographic_SPDF <- SpatialPointsDataFrame(coords= geographic[,c("LONGITUDE", "LATITUDE")], data=geographic[, c("PRCP","TAVG","DATE")], proj4string = CRS("+init=epsg:4326"))

geographic_SPDF <- spTransform(geographic_SPDF, crs(mdg_adm_2))

#Sort points into adminstrative zones
mdg_adm_2_geo_points <-over(geographic_SPDF,mdg_adm_2)

geographic <- mutate(geographic, Province=mdg_adm_2_geo_points$NAME_2)

#date
geographic$DATE<- as_date(geographic$DATE)

#plotting
geographic %>%
  ggplot(aes(x=DATE))+
  geom_line(aes(y=TAVG, color="TAVG",group=1))+
  geom_line(aes(y=PRCP/10, color="PRCP",group=1))+
  facet_wrap(~Province)+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Precipitation(mm)"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Average Temperature(C)")
# There aren't any significant climate variations right before and during the outbreak in different regions. The temperature in the most infected regions is around 25~30 Celsius around the outbreak period, favoring the activities of rat fleas. The rainfall condition is generally dry in Madagascar.

##Other covariants
#Merge cases data frame with spatial polygon data frame
SPDF_10202017<- merge(mdg_adm_2, total_10202017, by.x="NAME_2", by.y="Name", all.x=TRUE)
SPDF_11102017<- merge(mdg_adm_2, total_11132017, by.x="NAME_2", by.y="Name", all.x=TRUE)

#Density~ Prevalence
SPDF_10202017@data%>% 
  ggplot(aes(x=pop_density, y=Prevalence))+
  geom_point()+
  geom_smooth(method="glm")+
  xlab("Population density (per km^2)")+
  ylab("Prevalence(cases per thousand)")
SPDF_10202017@data %>%
  glm(Prevalence ~ pop_density, data=.)
#There is a linear relationship between population density and plague prevalence.

#Health coverage ~ Mortality
SPDF_10202017@data %>% 
  ggplot(aes(x=travel_time, y=Mortality))+
  geom_point()

SPDF_11102017@data %>%
  ggplot(aes(x=travel_time, y=Mortality))+
  geom_point()

SPDF_11102017@data %>%
  glm(Mortality ~ travel_time, data=.)
#Interesting finding: As the outbreak proceeded to its end, the correlation between health coverage and mortality became more linear. I assume the improvement of Madagascar's health system and medical assistance during the outbreak gradually increased the effectiveness of medical care there. 


