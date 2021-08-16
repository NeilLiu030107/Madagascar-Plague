#Loading libraries
library(tidyverse)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(lubridate)
library(sf)
library(spdep)


###Part 1: Read data

#Read cases by regions and hospitals from 10/20/2017 to 11/13/2017
plague <- read.csv("Data/Disease_data.csv")
View(plague)

#Read population data
population <- read.csv("Data/Population.csv")

#Adding prevalence and mortality columns to plague.csv
plague <-inner_join(plague, population, by="Name")

plague <-mutate(plague, Prevalence = plague$Undertreatment / plague$Population*1000, Fatality=plague$cumulative.death/(plague$cumulative.healed+plague$cumulative.death))

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
dist_mdg_mask <- raster("Data/travelTime.tif")

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
  select(Name, Total, Prevalence, Fatality)
head(total_10202017)

#11/13/2017 cases
total_11132017 <- plague %>%
  subset(Date=="11/13/2017"& Type=="Region") %>%
  select(Name, Total, Prevalence, Fatality)
head(total_10202017)

#Madagascar Provincial Map
mdg_adm_2 <- raster::getData("GADM", country="MDG", level=2)
mdg_provincial <-raster::getData("GADM", country="MDG", level=2)
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
# The average temperature didn't vary a lot throughout the year and across different regions. The temperature in the most infected regions is around 25~30 Celsius around Oct. 2017, favoring the activities of rat fleas,the important vector of plague. The rainfall in all recorded regions is at a low point around Oct. 2017 comparing with other months. This dry climatic condition is optimal to the flourishing of rat fleas.

##Other covariants
#Merge cases data frame with spatial polygon data frame
SPDF_10202017<- merge(mdg_adm_2, total_10202017, by.x="NAME_2", by.y="Name", all.x=TRUE)
SPDF_11102017<- merge(mdg_adm_2, total_11132017, by.x="NAME_2", by.y="Name", all.x=TRUE)
    
glm_prevalence <-glm(Prevalence ~ night_light + pop_density + elevation, data=SPDF_11102017@data, family=binomial())
summary(glm_prevalence)

#All four covariants have extremely big p-values, indicating that the data set is so small that accurate predictions and models can not be made. 

#Density~ Prevalence
SPDF_10202017@data%>% 
  ggplot(aes(x=pop_density, y=Prevalence))+
  geom_point()+
  geom_smooth(method="glm")+
  xlab("Population density (per km^2)")+
  ylab("Prevalence(cases per thousand)")

mod<-glm(Prevalence ~ pop_density, SPDF_10202017@data,family=binomial())
summary(mod)
#The p-value is 0.75, which rejects the alternative hypothesis that there is a correlation between population density and the prevalence of plague.

#Health coverage ~ Mortality
SPDF_10202017@data %>% 
  ggplot(aes(x=travel_time, y=Fatality))+
  geom_point()+
  geom_smooth(method="glm")+
  xlab("Travel time to health facilities(min)")+
  ylab("Fatality")

mod2 <-glm(Fatality ~ travel_time, data=SPDF_10202017@data, family=binomial())
summary(mod2)

SPDF_11102017@data %>%
  ggplot(aes(x=travel_time, y=Fatality))+
  geom_point()+
  geom_smooth(method="glm")+
  xlab("Travel time to health facilities(min)")+
  ylab("Mortality")

mod3 <-glm(Fatality ~ travel_time, data=SPDF_11102017@data, family=binomial())
summary(mod3)
#Again, the p-values are 0.749 and 0.827, which is too big to accept the alternative hypothesis that there's a linear relationship between health coverage and the mortality of plague in different regions.

#autocorrelation test
#data with average prevalence and fatality over whole time frame 
data <- plague %>%
  group_by(Name) %>%
  summarise(Prevalence=mean(Prevalence), Fatality=mean(Fatality))
  
data$Fatality[which(is.nan(data$Fatality))]<-0
data_spatial <- sp::merge(mdg_provincial, data, by.x="NAME_2", by.y="Name", all.x=TRUE)

data_spatial@data$Prevalence[which(is.na(data_spatial@data$Prevalence))] <-0

data_spatial@data$Fatality[which(is.na(data_spatial@data$Fatality))] <-0

# define contiguity neighbors
data_nb <- poly2nb(mdg_provincial)

#set weights
data_w <-nb2listw(data_nb)

#moran test
moran.test(data_spatial@data$Prevalence,listw = data_w)

moran.test(data_spatial@data$Fatality,listw = data_w)
#In the Moran's test of prevalence, it produces a p-value of 0.004 and a positive Moran I statistic value, which indicates there's the evidence of spatial autocorrelation for the distribution of plague prevalence. It statistically supports that there's clustering of plague cases near the central and central east coast areas in Madagascar, which is observed in the visualization part.
#In the contrary, it produces a p-value greater than 0.05 regarding to fatality, indicating there's no evidence of spatial clustering for the distribution of plague fatality across regions.
#However, the size of the sample is less than 30, so the result's reliability is contested.


#Discussion: The statistical modelling of Madagascar plague has two goals: visualizing the epidemic and exploring the impact of various factors in its spread. This research achieves success in visualization while making some progress toward the latter one.

#The great availability of disease, environmental, and social data makes the statistical analysis feasible. All geo-spatial data are obtained from online sources and can be easily processed using R-language. However, it is also noticed that the data recording and sharing practice between local government department and the WHO was poor. The disease data from WHO is incomplete and can't be used in statistic analysis. The data is retrieved and organized from government reports manually, which is inconvenient to individual researchers. The government reports under-reported plague cases and only covers the cases in the late half of the outbreak comparing with WHO data, which creates great disruption to statistical analysis.

#This research visualizes the outbreak using leaflet package and spatial data frame. The total case numbers of plague are plotted on a map over different administrative zones. By inputting dataframes of different dates, it's possible to observe the trend of the outbreak throughout the time frame. This research discusses the geographic distribution of the plague and its spreading patterns in different regions. 

#This research makes some progress in exploring the correlation between environmental and social factors and the characteristics of the plague epidemic. The temperature and rainfall generally favored the proliferation of rat fleas, which propelled the plague outbreak around October, 2017. The modelling of other covariates such as population density meets roadblocks. It is possibly because the small number of data (11 administrative zones) limits the accuracy of the models. It illustrates that large area data are not effective when doing spatial modelling. The solution may rely on recording disease prevalence and incidence on a smaller and accurate scale, such as smaller areas or points.

#This research discovers the spatial pattern of epidemic spread during the 2017 Madagascar plague outbreak and the effect of climate on the spread. This will hopefully provide a simple insight of plague control and prevention in Madagascar and the urgency to improve the disease monitoring system in underdeveloped countries. In terms of research, it shows the great potential of modern tools and methods when applying interdisciplinary knowledge to real case scenarios.     
 

