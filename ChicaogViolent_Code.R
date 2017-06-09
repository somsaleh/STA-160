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
install.packages("data.table")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("leaflet")
install.packages("tidyr")
install.packages("RgoogleMaps")
install.packages("maptools")
install.packages("ggmap")


library(data.table)
library(RColorBrewer)
library(lattice)
library(plotly)
library(leaflet)
library(tidyr)
library(lattice)
library(RgoogleMaps)
library(maptools)
library(ggmap)
library(ggplot2)



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




## Barchart Analysis

# Step 1) 
#  * Table to Dataframe Function * # 

VType = function(VTypeName, dataVar1, dataVar2){
  # create table by year
  VTypeName = table(dataVar1, dataVar2)   
 
   # create dataset for the table
  VTypeName = data.frame(VTypeName)
  
  # change column names
  colnames(VTypeName) = c("Area", "Year", "Count") 
  
  ## Pivot the table 
  Pivot = spread(VTypeName, Year, Count) 
  Pivot
}


# Step 2
# Now create the pivoted data frames for each violent crime
BPivot = VType(BType, Battery$`Community Area` , Battery$Year)   # Battery
APivot = VType(AType, Assault$`Community Area` , Assault$Year)   # Assault
HPivot = VType(HType, Homicide$`Community Area`, Homicide$Year)  # Homicide
RPivot = VType(RType, Robbery$`Community Area` , Robbery$Year)   # Robbery 


#Step 3
#  * Barchart Function * # 
# The function will later be used for the 4 diff violent crimes

ChartFunc =  
  # The function will read in each pivoted df
  # input each Var1 etc represent each var per year in the df
  function(chartName, titleName, data, dataVar1, dataVar2,
                      dataVar3, dataVar4, dataVar5, dataVar6,
                      dataVar7, dataVar8, dataVar9) { 
  
  chartName = 
    
    # use Plotly functions
    plot_ly(data, x = dataVar1) %>%
    add_bars(y = dataVar2, name = "2010") %>% # allows you to continue and start new line 
    add_bars(y = dataVar3, name = "2011", visible = FALSE) %>% # add each bar for each year
    add_bars(y = dataVar4, name = "2012", visible = FALSE) %>%
    add_bars(y = dataVar5, name = "2013", visible = FALSE) %>%
    add_bars(y = dataVar6, name = "2014", visible = FALSE) %>%
    add_bars(y = dataVar7, name = "2015", visible = FALSE) %>%
    add_bars(y = dataVar8, name = "2016", visible = FALSE) %>%
    add_bars(y = dataVar9, name = "2017", visible = FALSE) %>%
    layout(
      # Set up the layout 
      title = titleName, 
      xaxis = list(domain = c(0.1, 1)),
      yaxis = list(title = "Frequency"),
      updatemenus = list(
        list(
          y = 0.7,
          
          # Set up the drop down button per year
          # this will allow the user to control which year to view at a time
          
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
  chartName
}

# Step 4
# Output for the barchart per Violent Crime

AssaultChart = 
ChartFunc(AssaultChart2, "Assault Crime per Community Area",  
          APivot, APivot$Area, APivot$`2010`, APivot$`2011`,
          APivot$`2012`, APivot$`2013`, APivot$`2014`, 
          APivot$`2015`, APivot$`2016`, APivot$`2017`)

BatteryChart = 
  ChartFunc(BatteryChart2, "Battery Crime per Community Area",
            BPivot, BPivot$Area, BPivot$`2010`, BPivot$`2011`,
            BPivot$`2012`, BPivot$`2013`, BPivot$`2014`, 
            BPivot$`2015`, BPivot$`2016`, BPivot$`2017`)

HomicideChart = 
  ChartFunc(HomicideChart2, "Homicide Crime per Community Area",
            HPivot, HPivot$Area, HPivot$`2010`, HPivot$`2011`,
            HPivot$`2012`, HPivot$`2013`, HPivot$`2014`, 
            HPivot$`2015`, HPivot$`2016`, HPivot$`2017`)

RobberyChart = 
  ChartFunc(RobberyChart2, "Robbery Crime per Community Area",
            RPivot, RPivot$Area, RPivot$`2010`, RPivot$`2011`,
            RPivot$`2012`, RPivot$`2013`, RPivot$`2014`,
            RPivot$`2015`, RPivot$`2016`, RPivot$`2017`)

AssaultChart
BatteryChart
HomicideChart
RobberyChart

 
 
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

















# Resources Used:
# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r
# https://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id
#https://plot.ly/r/dropdowns/

# Read in the Shape File












