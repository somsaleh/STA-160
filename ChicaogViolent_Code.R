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

install.packages("data.table")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("sonify")
install.packages("tidyr")
install.packages("zoo")

library(data.table)
library(RColorBrewer)
library(lattice)
library(plotly)
library(zoo)
library(sonify)
library(tidyr)
library(lattice)


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


#rob = VType[VType$Type == "ROBBERY",]
#battery = VType[VType$Type == "BATTERY",]
#homi = VType[VType$Type == "HOMICIDE", ]
#assault = VType[VType$Type == "ASSAULT",]
# 

#plot(sort(rob$date), rob$Count, type = "l")

#plot_ly(rob, x = sort(rob$date), y = rob$Count, type ="scatter", mode = "lines")
#add_trace(battery,  x = sort(battery$date), y = battery$Count, type ="scatter", mode = "lines")


# Tidy R  pivot the data frame
VPivot = spread(VType, Type, Count)


VPivot2 = VPivot[order(VPivot$date), ]


sapply(VPivot2, class)
plot_ly(VPivot2, x = VPivot2$date, y = VPivot2$ASSAULT, 
      type = "scatter", mode = "lines", name = "Assault", line = list(color = 'rgba(67,67,67,1)', width = 2)) %>%
      add_trace(y = VPivot2$BATTERY, name = "Battery", modes = "lines", line = list(color = "dark blue"), width =2 ) %>%
  
  add_trace(y = VPivot2$HOMICIDE, name = "Homicide", modes = "lines", line = list(color = "purple")) %>%
  add_trace(y = VPivot2$ROBBERY, name = "Robbery", modes = "lines", line = list(color = "blue"))



# Barplot 







# spreading 
# plot(sort(VType$date), VType$Count, type = "l")









# Resources Used:
# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r


