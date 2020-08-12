getwd()
setwd("/Users/ritahnfamba/Downloads")
mydata <- read.csv("bus_data.csv")
mydata
#1 a Are there any attributes in the data set that are not useful for analysis or 
#making predictions? why?
#Answer; yes. the data set has some categorical data that is not useful for 
#analysis such as predictable which has a boolean value, dirTag - has both 
#numeric & character values & finally speedKmHr which is just zeros. 
#b Do any of the variables appear to be treated as discreet even though they actually 
#represent continuous values?
#Ans: Yes.  lastTime and time attributes
#c do any of the variables seem to represent the same data?
#And: Yes; lastTime and time have the same data
#d What do you think the dirTag attribute represents?
#Ans: I think dirTag represents possible routes that the buses can take 
#2 Now inspect the distribution of route numbers. Are there any bus routes that 
#should not be included in our 1 bus analysis? If so, remove those.
#Yes there are rows that are duplicates; these shall be removed from the data frame.
distinct(mydata)
library(dplyr)
select(mydata, -c(predictable, speedKmHr, dirTag, epoch, lastTime, local, secsSinceReport, time, utc)) 
#b Is there anything else we should filter that is not a complete attribute?
#We need to eliminate rows that contain NA values in the data set 
newData <-na.omit(mydata)
#3a Which attributes should be excluded from analysis? 
#Ans: lat & lon attributes should be excluded
#Are any of them perfectly correlated? Should we include all of those?
#yes we could include epoch, routeTag, vehicleld, heading
pairs(~ epoch + vehicleId + heading + secondsPastMidnight + secsSinceReport, data = newData)
pairs
#Rename D#routeTag to be readable in R
#3b If you want to ‘label’ points in your analysis with the direction the buses are heading, 
#which attribute do you need?
#Ans: heading attribute 
#4 Select Data (10pts)
#Filter out the rows that have irrelevant or undefined values for dirTag, and those 
#that have any routeTag other than 1.
#Rename D#routeTag as routeTag
library(dplyr)
newData = rename(newData, routeTag = D.routeTag)
#Filtering undefined values of routeTag
newData1 = filter(newData, routeTag %in% c(1))
#converting data
dts = c(newData$epoch, newData$local, newData$utc)
mydates = dts
class(mydates) = c('POSIX')
mydates
#5
#View the route (15pts)
#Choose features that will help you plot the geographic locations of the route on a graph. Save the graph you produce.
#Attributes include lat & lon 
library(ggplot2)
library(maps)
#load new map data
newmap <- map_data("state")
newmap <- ggplot()
newmap <- newmap + geom_polygon( data= mydata, aes(x=lon, y=lat),colour="white", fill="grey10")
newmap
#How closely does this mesh with the actual bus route? Can you guess what is happening when there are any deviations from the actual route? 
  
  
#6
#Bus time and frequency (15pts)
#We want to understand when the buses run and their frequency throughout the 24 hour period. 
#a Plot out the frequency of the bus observations by two hour increments. 
#Save the graph you produce.
hist(mydata$secondsPastMidnight/3600)
#What are the peak periods when the buses run most frequently?  
#Hint: You can view the number of rows that appear by time interval, and then you can make the number of different time intervals 12 instead of the default value, representing 2 hour windows.
#Ans: Peak periods between 4-8pm
#7
#run pca
library(factoextra)
library(ggplot2)

#7
#Correlation
plot(mydata$vehicleId, mydata$secondsPastMidnight)
fit <- lm(vehicleId ~ secondsPastMidnight, data = newData)
abline(fit, col = "red")
bus.regression <- lm(secondsPastMidnight ~ vehicleId, data = newData)
summary(regression)
#run pca
mydata.pca <- prcomp(mydata, center = TRUE, scale. = TRUE)

