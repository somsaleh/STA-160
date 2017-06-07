# Chicago Crime 


# List of Resources Used





# set the working directory
wd = setwd("/Users/Megatron/Documents/UCD/Spring 2017/STA-160/Crime Project/Datasets/Chicago")

ChicagoCrime = readRDS("ChicagoCrime.rds")
dim(ChicagoCrime) #  2264846 x 24

Violent = readRDS("ViolentCrime.rds")
dim(Violent) # 637490 x 24
sapply(Violent, class)

# set and dl library
install.packages("audiolyzR")
install.packages("data.table")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("sonify")
install.packages("leaflet")
install.packages("tidyr")
install.packages("zoo")
install.packages("RgoogleMaps")
install.packages("readOGR")
install.packages("maptools")
install.packages("ggmap")
install.packages("rgdal")

library(audiolyzR)
library(data.table)
library(RColorBrewer)
library(lattice)
library(plotly)
library(zoo)
library(sonify)
library(leaflet)
library(tidyr)
library(lattice)
library(RgoogleMaps)
library(maptools)
library(ggmap)
library(ggplot2)
library(readOGR)
library(rgdal)
library(gpclib)
library(raster)





##               Time Series Plot for Violent Crimes        ##


# Step 1)  group by Month and Year and create a new variable
Violent$MonYr =  format(Violent$Date, "%m/%Y")

# check the class
sapply(Violent, class)

# Step 2) Date, Crime, Count create a database
VType = table(Violent$`Primary Type`, Violent$MonYr)
VType = data.frame(VType)
colnames(VType) = c("Type", "Date", "Count")

VType$Date = as.character(VType$Date)
g = strsplit(VType$Date, "/")
VType$date = sapply(g, function(x) {
  paste0(x[2], "-", x[1], "-01")
  })

VType$date = as.Date(VType$date)

# Tidy R  pivot the data frame
# 
VPivot = spread(VType, Type, Count)

VPivot2 = VPivot[order(VPivot$date), ]

sapply(VPivot2, class)

# Create the Time Series plot
TimeSeriesPlot = 
  plot_ly(VPivot2, x = VPivot2$date, y = VPivot2$ASSAULT, 
      type = "scatter", mode = "lines", name = "Assault", line = list(color = 'rgba(67,67,67,1)', width = 2)) %>%
      # add the remaining response variables variables to the plot
      add_trace(y = VPivot2$BATTERY, name = "Battery", modes = "lines", line = list(color = "dark blue"), width = 2 ) %>%
      add_trace(y = VPivot2$HOMICIDE, name = "Homicide", modes = "lines", line = list(color = "purple")) %>%
     add_trace(y = VPivot2$ROBBERY, name = "Robbery", modes = "lines", line = list(color = "blue"))



# Density Plots
density()



# Barplot 






# Section 2:  Mapping The Data

CArea = readShapePoly("geo_export_1cecd1d2-ae2d-4c08-a216-6154cbcc2077")
class(CArea)
colors <- brewer.pal(9, "BuGn")

area.points <- fortify(CArea) # creating the dataframe for the shape file
class(area.points)
sapply(area.points, class)

CHi = get_map("Chicago, IL")
ggmap(CHi)

#

#  shp = fortify(as(shp, "Spatial"), region = "community")

ggmap(CHi) + geom_polygon(aes(x = long,
                   y = lat,
                   group = group, # fill = id) inside ,
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

#  Second way to read in shape file, 
a = readShapeSpatial("geo_export_1cecd1d2-ae2d-4c08-a216-6154cbcc2077")
# plot the shape 
plot(a, axes = TRUE, border = "black")

# add the labels to the map , use as reference
text(coordinates(CArea), labels = CArea$area_numbe)


# Combine the Spatial Data with the Crime Data

# now combine the shape file with the data
  vc = subset(Violent, Year == "2010")
  vc = subset(vc, vc$`Primary Type` == "BATTERY")
     vc$count <- 1
  
#Subsetting data
lonlat.df10 <- data.frame(lat = vc$lat, lon = vc$lon)


#Convert crime data to a spatial points object
#vc1 <- SpatialPointsDataFrame(coords= vc[, c("longitude", "latitude")],
                                 # data=vc[, c("Year", "Community", "type", "arrest")],)

chi_spdf10 <- SpatialPointsDataFrame(coords = lonlat.df10[, c("lon", "lat")],
                                    data= vc)
vc_tract <- over(x=vc, y=tract) 
     
    

# Animated Plot


## The animated plot 
#plot_geo(Violent) %>%
add_trace ( 
  x = Violent$Longitude, y = Violent$Latitude, 
  z = Violent$`Primary Type` ,  colors = "Blues", 
  text = "idk", marker = list(line = l)) %>%
  colorbar( title = "Chicago Crime")

     
     
     
  
  #tract =  shapefile("geo_export_1cecd1d2-ae2d-4c08-a216-6154cbcc2077")
  #tract <- spTransform(x=tract, CRSobj=CRS("+proj=longlat +datum=WGS84"))





# spreading 
# plot(sort(VType$date), VType$Count, type = "l")






# Resources Used:
# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r
# https://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id

# Read in the Shape File












