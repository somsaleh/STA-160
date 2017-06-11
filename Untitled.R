#Exploratory plots for Los Angeles Crime 

#Subset the data by type of violent crime
la_assault = subset(la_violent, `Crime_Code_Desc` == "ASSAULT WITH DEADLY WEAPON ON
                     POLICE OFFICER"|`Crime_Code_Desc` == "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT")
la_assault$Crime_Code_Desc = "ASSAULT"
la_battery = subset(la_violent, `Crime_Code_Desc` == "BATTERY - SIMPLE ASSAULT"|
                       `Crime_Code_Desc` == "BATTERY ON A FIREFIGHTER"|
                       `Crime_Code_Desc` == "BATTERY POLICE (SIMPLE)"|                                 
                       `Crime_Code_Desc` == "BATTERY WITH SEXUAL CONTACT")
la_battery$Crime_Code_Desc = "BATTERY"
la_homicide = subset(la_violent, `Crime_Code_Desc` == "CRIMINAL HOMICIDE"|
                       `Crime_Code_Desc` == "MANSLAUGHTER, NEGLIGENT")
la_homicide$Crime_Code_Desc = "HOMICIDE"
la_robbery = subset(la_violent, `Crime_Code_Desc` == "ROBBERY")

#Combine the above dataframes
crimetypes <- list(la_assault,la_battery,la_homicide,la_robbery)
la_violent2 = crimetypes[[1]]
for (i in 2:length(crimetypes)) {
  la_violent2 = rbind(la_violent2, crimetypes[[i]])
  i = i+1
}

#Installing packages
library("data.table")
library("RColorBrewer")
install.packages("plotly")
install.packages("leaflet")

library(lattice)
library(plotly)
library(leaflet)
library(tidyr)
library(RgoogleMaps)
library(maptools)
library(ggmap)
library(ggplot2)


## Time Series Plot for Violent Crimes in Los Angeles

#Step 1) Group by month and year and create a new variable
Mon = substr(la_violent2$Date.Occurred, 1,2)
MonYr = paste(Mon, la_violent2$Year, sep = "/")
la_violent2["MonYr"] <- MonYr

#Step 2) Date, Crime, Count - create a database
VType = table(la_violent2$`Crime_Code_Desc`, la_violent2$MonYr)
VType = data.frame(VType)

#Change the column names
colnames(VType) = c("Type", "Date", "Count")

#Change the class of the variable 
VType$Date = as.character(VType$Date)

#Create a function to create a new date column
#Original date variable is month/year
#We want to change it month/1/year
#Reason: way to aggregate the data

monthly = function(data_col) {
  split_dates = strsplit(data_col, "/")
  new_dates = sapply(split_dates, function(x) {
    new_date = paste0(x[2], "-", x[1], "-01")
    new_date
  })
  as.Date(new_dates)
}

VType$Date = monthly(VType$Date)

#Tidy R  pivot the data frame
VPivot = spread(VType, Type, Count)

#Order the dates
VPivot = VPivot[order(VPivot$Date), ]

# Create the Time Series plot
tsplot = plot_ly(VPivot, x = VPivot$Date, y = VPivot$ASSAULT, 
          type = "scatter", mode = "lines", name = "Assault", line = list(color = 'rgba(67,67,67,1)', width = 2)) %>%
  #Add the remaining response variables to the plot
  add_trace(y = VPivot$BATTERY, name = "Battery", modes = "lines", 
            line = list(color = "dark blue"), width = 2 ) %>%
  add_trace(y = VPivot$HOMICIDE, name = "Homicide", modes = "lines", 
            line = list(color = "purple")) %>%
  add_trace(y = VPivot$ROBBERY, name = "Robbery", modes = "lines", 
            line = list(color = "blue"))

## Barchart Analysis

# Step 1) Table to Dataframe Function

VType = function(VTypeName, dataVar1, dataVar2){
  #Create table by year
  VTypeName = table(dataVar1, dataVar2)   
  
  #Create dataset for the table
  VTypeName = data.frame(VTypeName)
  
  #Change column names
  colnames(VTypeName) = c("Area", "Year", "Count") 
  
  #Pivot the table 
  Pivot = spread(VTypeName, Year, Count) 
  Pivot
}


#Step 2)
#Now create the pivoted data frames for each violent crime by area
BPivot = VType(BType, la_battery$`Area.Name`, la_battery$Year)   # Battery
APivot = VType(AType, la_assault$`Area.Name`, la_assault$Year)   # Assault
HPivot = VType(HType, la_homicide$`Area.Name`, la_homicide$Year) # Homicide
RPivot = VType(RType, la_robbery$`Area.Name`, la_robbery$Year)   # Robbery 

#Step 3) Barchart Function
# The function will later be used for the 4 diff types violent crimes
ChartFunc =  
  #The function will read in each pivoted df
  #Input each var1 etc; represent each var per year in the df
  function(chartName, titleName, data, dataVar1, dataVar2,
           dataVar3, dataVar4, dataVar5, dataVar6,
           dataVar7, dataVar8, dataVar9) { 
    
    chartName = 
      #Use Plotly functions
      plot_ly(data, x = dataVar1) %>%
      add_bars(y = dataVar2, name = "2010") %>% # Allows you to continue and start new line 
      add_bars(y = dataVar3, name = "2011", visible = FALSE) %>% # Add each bar for each year
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

#Step 4) Output for the barchart per violent crime
AssaultChart = 
  ChartFunc(AssaultChart2, "Assault Crime per Area",  
            APivot, APivot$Area, APivot$`2010`, APivot$`2011`,
            APivot$`2012`, APivot$`2013`, APivot$`2014`, 
            APivot$`2015`, APivot$`2016`, APivot$`2017`)

BatteryChart = 
  ChartFunc(BatteryChart2, "Battery Crime per Area",
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

# Plotting time series plot for socioeconomic variables
socioeconla <- read.csv("SocioEconLA.csv")
socioeconla <- socioeconla[,c("Year","PopEst","HSEnr","MedIncome","Poverty","Unemp.1")]

## Time Series Plot for Socioeconomic Factors in Los Angeles

# Create the Time Series plot
tsplot2 = plot_ly(socioeconla, x = socioeconla$Year, y = socioeconla$PopEst, 
                 type = "scatter", mode = "lines", name = "Population Estimate", line = list(color = 'rgba(67,67,67,1)', width = 2)) %>%
  #Add the remaining response variables to the plot
  add_trace(y = socioeconla$HSEnr, name = "High School Enrollment", modes = "lines", 
            line = list(color = "dark blue"), width = 2 ) %>%
  add_trace(y = socioeconla$MedIncome, name = "Median Household Income ($)", modes = "lines", 
            line = list(color = "purple")) %>%
  add_trace(y = socioeconla$Poverty, name = "Population in Poverty", modes = "lines", 
            line = list(color = "blue"))
  add_trace(y = socioeconla$Unemp.1, name = "Unemployed (16+)", modes = "lines", 
          line = list(color = "blue"))