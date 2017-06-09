# Chicago Crime 


# List of Resources Used




# set the working directory
wd = setwd("/Users/Megatron/Documents/UCD/Spring 2017/STA-160/Crime Project/Datasets/Chicago")

ChicagoCrime = readRDS("ChicagoCrime.rds")
dim(ChicagoCrime) #  2264846 x 24

Violent = readRDS("ViolentCrime.rds")
dim(Violent) # 637490 x 24
sapply(Violent, class)

# Subset the data by type of Violent Crime
Assault  = subset(Violent, Violent$`Primary Type` ==  "ASSAULT")
Battery  = subset(Violent, Violent$`Primary Type` ==  "BATTERY")
Homicide = subset(Violent, Violent$`Primary Type` ==  "HOMICIDE")
Robbery  = subset(Violent, Violent$`Primary Type` ==  "ROBBERY")


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

# Change the column names
colnames(VType) = c("Type", "Date", "Count")

# Change the class of the variable 
VType$Date = as.character(VType$Date)

# Create a function to create a new date column
# original Date varaible is month/year
# we want to change it month/1 ( day)/ year
# reason: way to aggregate the data

monthly = function(data_col) {
  split_dates = strsplit(data_col, "/")
  new_dates = sapply(split_dates, function(x) {
    new_date = paste0(x[2], "-", x[1], "-01")
    new_date
  })
  as.Date(new_dates)
}

VType$date = monthly(VType$Date)

# Tidy R  pivot the data frame
VPivot = spread(VType, Type, Count)

# order the dates
VPivot = VPivot[order(VPivot$date), ]

# check the class
sapply(VPivot, class)

# Create the Time Series plot
TimeSeriesPlot = 
  plot_ly(VPivot, x = VPivot$date, y = VPivot$ASSAULT, 
      type = "scatter", mode = "lines", name = "Assault", line = list(color = 'rgba(67,67,67,1)', width = 2)) %>%
      # add the remaining response variables variables to the plot
      add_trace(y = VPivot$BATTERY, name = "Battery", modes = "lines", 
                line = list(color = "dark blue"), width = 2 ) %>%
      add_trace(y = VPivot$HOMICIDE, name = "Homicide", modes = "lines", 
                line = list(color = "purple")) %>%
     add_trace(y = VPivot$ROBBERY, name = "Robbery", modes = "lines", 
               line = list(color = "blue"))




## Barplot 

# Battery Analysis # 
 
 #  Creating a table
# BType = table(Battery$`Community Area`, Battery$MonYr) # by month
 
 BType = table(Battery$`Community Area`, Battery$Year) # by year

 BType = data.frame(BType) # create the dataset for by year 
 
 colnames(BType) = c("Area", "Year", "Count") # rename the columns
 
# BType$Date = as.character(BType$Date) # change the class of the variable
 
 # run the function to get aggregated date (only use this when BType is table of MonYr )
 #BType$date = monthly(BType$Date) 
 
 # Pivot the table by Year
 BPivot = spread(BType, Year, Count)
 #BPivot  = BPivot[order(BPivot$date), ]
 #BType  = BType[order(BType$date), ]
 
 
#  Create the Chart
 BatteryChart  = 
        
 plot_ly(BPivot, x = BPivot$Area) %>%
   add_bars(y = sort(BPivot$`2010`, decreasing = TRUE), name = "2010") %>%
   add_bars(y = sort(BPivot$`2011`, decreasing = TRUE), name = "2011", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2012`, decreasing = TRUE), name = "2012", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2013`, decreasing = TRUE), name = "2013", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2014`, decreasing = TRUE), name = "2014", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2015`, decreasing = TRUE), name = "2015", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2016`, decreasing = TRUE), name = "2016", visible = FALSE) %>%
   add_bars(y = sort(BPivot$`2017`, decreasing = TRUE), name = "2017", visible = FALSE) %>%
   layout(
     title = "Battery In Community Areas Per Year",
     xaxis = list(domain = c(0.1, 1)),
     yaxis = list(title = "Frequency"),
     updatemenus = list(
       list(
         y = 0.7,
         buttons = list(
           list(method = "restyle",
                args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                label = "2010"),
           list(method = "restyle",
                args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                label = "2011"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                label = "2012"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
                label = "2013"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
                label = "2014"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
                label = "2015"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
                label = "2016"),
           list(method = "restyle",
                args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
                label = "2017")))
     )
   )
        
 
########

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

ggmap(CHi) + geom_polygon(aes(x = long, y = lat,
                   group = group), # fill = id) inside ,
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

















#  Second way to read in shape file, 
#a = readShapeSpatial("geo_export_1cecd1d2-ae2d-4c08-a216-6154cbcc2077")
# plot the shape 
#plot(a, axes = TRUE, border = "black")

# add the labels to the map , use as reference
#text(coordinates(CArea), labels = CArea$area_numbe)


# Combine the Spatial Data with the Crime Data

# now combine the shape file with the data
  #vc = subset(Violent, Year == "2010")
  #vc = subset(vc, vc$`Primary Type` == "BATTERY")
   #  vc$count <- 1
  
#Subsetting data
#lonlat.df10 <- data.frame(lat = vc$lat, lon = vc$lon)


#Convert crime data to a spatial points object
#vc1 <- SpatialPointsDataFrame(coords= vc[, c("longitude", "latitude")],
                                 # data=vc[, c("Year", "Community", "type", "arrest")],)

#chi_spdf10 <- SpatialPointsDataFrame(coords = lonlat.df10[, c("lon", "lat")],
                                    #data= vc)
#vc_tract <- over(x=vc, y=tract) 
     
    

# Animated Plot


## The animated plot 
#plot_geo(Violent) %>%
#add_trace ( 
  #x = Violent$Longitude, y = Violent$Latitude, 
  #z = Violent$`Primary Type` ,  colors = "Blues", 
  #text = "idk", marker = list(line = l)) %>%
  #colorbar( title = "Chicago Crime")

     
     
     
  
  #tract =  shapefile("geo_export_1cecd1d2-ae2d-4c08-a216-6154cbcc2077")
  #tract <- spTransform(x=tract, CRSobj=CRS("+proj=longlat +datum=WGS84"))





# spreading 
# plot(sort(VType$date), VType$Count, type = "l")






# Resources Used:
# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r
# https://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id
#https://plot.ly/r/dropdowns/

# Read in the Shape File












