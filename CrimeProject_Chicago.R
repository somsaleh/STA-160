# STA 160  - Crime Data Project
# Purpose of Project: To look at crimes and trends. 
#                     Have crimes reduced over the years? Yes or no, and why?
#  This R-Script only represents data cleaning, manipulation, and subsetting the data that will later 
#  be focused on only. 



# Chicago Dataset

# set the working directory
wd = setwd("/Users/Megatron/Documents/UCD/Spring 2017/STA-160/Crime Project/Datasets/Chicago")


# set and dl library

install.packages("data.table")
install.packages("RColorBrewer")


library(data.table)
library(RColorBrewer)
library(lattice)



##                     Original Data and Readjusting it ###
# Read in the data
chicago = fread("Crimes_-_2001_to_present.csv") # returns both a data.table & data.frame 

Chicago = as.data.frame(chicago) # read in as a dataframe

# check the class
class(Chicago)

# check the head 
head(Chicago)

# check the class of variables
sapply(Chicago, class)   

# Only Test on Smaller Set To Split Date & Time
testdate = Chicago[1:10,]
testdate$time = substring(testdate$Date, 12, 22)
testdate$time =  as.Date(testdate$time, "%h/%m/%s") # ask about this

# Split the Date Column and Update Column
nchar(Chicago$Date) # 22 characters 1 - 10 for Date, 12-22 for Time 
nchar(Chicago$`Updated On`) # same as Date

Chicago$date = substring(Chicago$Date, 1, 10) # get the number of character
Chicago$time = substring(Chicago$Date, 12, 22)# to be used when splitting the column

Chicago$Update = substring(Chicago$`Updated On`, 1, 10) # number of character, same as above
Chicago$Update_time = substring(Chicago$`Updated On`, 12, 22)


# now change the class
Chicago$date  = as.Date(Chicago$date,"%m/%d/%Y")
Chicago$Update  = as.Date(Chicago$Update,"%m/%d/%Y")


# check the class of variables
sapply(Chicago, class)  

# range of Date
range(Chicago$date)   # 2001-01-01 , 2017-05-20
range(Chicago$Update) # 2006-03-31,  2017-05-27


# now lets subset 2010 - to current 
Chicago2010_present = subset(Chicago, Chicago$Year %in% c(2010:2017))
ChicagoCrime = Chicago2010_present

ChicagoCrime = ChicagoCrime[, c(-3,-19)] # remove Date and Updated On, since we already split the columns
colnames(ChicagoCrime)[22:24] = c("UpdateTime", "Date", "Time") # Change the Column Names

# write csv
write.csv(ChicagoCrime, file = "ChicagoCrime.csv")
saveRDS(ChicagoCrime, file = "ChicagoCrime.rds")


##      Section 2: Exploration of the Data ##
# table of crimes
top10 = sort(table(Chicago$`Primary Type`), decreasing = TRUE)[1:10]

# Theft               1314542 
# Battery             1153083
# CRIMINAL DAMAGE     726588
# NARCOTICS           690623
# OTHER OFFENSE       391820
# ASSAULT             386638
# BURGLARY            368687
# MOTOR VEHICLE THEFT 297719
# ROBBERY             238651
# DECEPTIVE PRACTICE  230832

total = sum(table(Chicago$Year)) # 6319745 crimes 2001 - 2017 Apr 22


# get the total for 2010 - 2017
total_crime = sum(table(ChicagoCrime$Year)) # 2243817 2010 - Apr 22 2017

# get the total count per year 
YearsCount = table(ChicagoCrime$Year) # total crimes per years

# get the percentage per year
YearsPercent = round(prop.table(table(ChicagoCrime$Year)),4) * 100 
# % breakdown of crimes
# 2010  2011    2012   2013   2014   2015   2016   2017 
# 370173 351589 335720 306740 274639 263190 267064  74702 



                    #  Section 3: Violent Crimes #
violent  = subset(ChicagoCrime, ChicagoCrime$`Primary Type` %in% 
                  c("ASSAULT", "BATTERY", "HOMICIDE", 
                    "ROBBERY", "SEXUAL ASSAULT", "WEAPONS VIOLENCE")) 

saveRDS(violent, file =  "ViolentCrime.rds")
dim(violent) #24 x 637490

total_vcrime = table(violent$Year)
lines(table(violent$`Primary Type`))
#2010   2011   2012   2013   2014   2015   2016   2017 
#101647  95288  93020  84216  76567  76086  81737  28929 




#familyOffenses = subset(ChicagoCrime, ChicagoCrime$`Primary Type` %in%



                    ##            Section 2 : Visualization           ##


##                                 Plotting the Data                        ## 
# Plot count of Overall Crimes
options(scipen=999)
plot(total_vcrime, type = "l")


 

##                                    Mapping the Data                    ##