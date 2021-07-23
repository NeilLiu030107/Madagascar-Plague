library(tidyverse)

library(leaflet)
library(sp)
library(rgdal)
library(raster)


#cumulative data
cumulative <- read.csv("https://raw.githubusercontent.com/systemsmedicine/plague2017/master/data/cum.csv")
View(cumulative)

#cases by regions and hospitals from 10/20/2017 to 11/13/2017
plague <- read.csv("Disease_data.csv")
View(plague)





#geographic info of main cities(temp, elevation, coordination, precipitation)
geographic <- read.csv("https://raw.githubusercontent.com/systemsmedicine/plague2017/master/data/1180122.csv")
View(geographic)

#Population density
pop_density <- raster("mdg_pd_2017_1km_UNadj.tif")

#night light
night_light <- raster("mdg_viirs_100m_2016.tif")



#Travel time to healthcare facilities
mdg_travel_time <- raster("2020_walking_only_travel_time_to_healthcare.geotiff")

mdg_adm_0 <- raster::getData("GADM", country="MDG", level=0) 
dist_mdg <- raster::crop(x = mdg_travel_time, y = mdg_adm_0)
dist_mdg_mask <- raster::mask(x = dist_mdg, mask = mdg_adm_0)
#plotting maps
par(mfrow=c(1,3))

plot(pop_density)
plot(night_light)
plot(dist_mdg_mask)

#leaflet image for population density

pal <- colorNumeric(c("#9ecae1","#FEE0D2","#DE2D26"), values(pop_density), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(pop_density, colors = pal, opacity = 0.8) %>%
  addLegend(pal=pal, values = values(pop_density), title="Population density")

#leaflet image for night light
night_lights_aggregated <- raster::aggregate(night_light, fact= 10)

pal2 <- colorNumeric(c("#fff5eb","#fdbe85","#fd8d3c","#e6550d","#7f2704"), domain=c(0,50), na.color = "transparent")
?colorNumeric

leaflet() %>% addTiles() %>%
  addRasterImage(night_lights_aggregated, colors = pal2, opacity = 0.8) %>%
  addLegend(pal=pal2, values = c(0,50), title="Night light")


#leaflet image for travel time to health facilities
pal3 <- colorNumeric("viridis", values(dist_mdg_mask), na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(dist_mdg_mask, colors = pal3, opacity = 0.8) %>%
  addLegend(pal=pal3, values = values(dist_mdg_mask), title="Travel time to health facilities")
  




# 10/20/2017 Total cases in regions
total_10202017 <- plague %>%
  subset(Date=="10/20/2017"& Type=="Region") %>%
  select(Name, Total)
head(total_10202017)

#11/13/2017 Total cases in regions
total_11132017 <- plague %>%
  subset(Date=="11/13/2017"& Type=="Region") %>%
  select(Name, Total)
head(total_10202017)

#Madagascar Provincial Map
mdg_adm_2 <- raster::getData("GADM", country="MDG", level=2)
str(mdg_adm_2)

#10/20/2017 Total cases by regions Map
total_10202017_Map <-sp::merge(mdg_adm_2, total_10202017, by.x="NAME_2", by.y="Name")
View(total_10202017_Map)
names(total_10202017_Map)

bins <-c(0,10,20,40,80,160,320,640,1280,Inf)
pal4 <- colorBin("YlOrRd", domain = total_10202017_Map$Total, bins = bins)
m1 <- leaflet(total_10202017_Map) %>%
  setView(46,-18,4) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal4(Total),
    color="white",
    opacity=1,
    fillOpacity = 0.7
    )

m1





