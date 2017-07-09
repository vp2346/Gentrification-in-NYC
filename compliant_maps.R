library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(googleVis)
require(igraph)
require(googleVis)
library(corrplot)
library(FSAdata)
library(plotrix)

library(withr)
library(devtools)
#install_github('arilamstein/choroplethrZip@v1.3.0')
library(choroplethrZip)


Data <- read.csv("The311.csv", header = TRUE)
Data_new <- Data[!is.na(Data$Incident.Zip),]
head(Data_new$Created.Date)
Data_new$year <- format(as.Date(Data_new$Created.Date, format = "%m/%d/%Y %I:%M:%S %p"), "%Y")
Data_new$year_factor <- as.factor(Data_new$year)
Data_new$zip <- as.factor(Data_new$Incident.Zip)
Data_fullzip <- Data_new[Data_new$zip != "",]
Data_7type <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Dirty Conditions' | Data_fullzip$Complaint.Type == 'Drinking'| Data_fullzip$Complaint.Type == ' Homeless Encampment'|Data_fullzip$Complaint.Type == 'Indoor Air Quality' | Data_fullzip$Complaint.Type == 'Mold' | Data_fullzip$Complaint.Type == 'Water Quality' | Data_fullzip$Complaint.Type == 'Urinating in Public'),]
Data_group <-Data_7type %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-7type.csv")

Data_Dirty <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Dirty Conditions'),]
Data_group <-Data_Dirty %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Dirty.csv")


Data_Drinking <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Drinking'),]
Data_group <-Data_Drinking %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Drinking.csv")

Data_Homeless <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Homeless Encampment'),]
Data_group <-Data_Homeless %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Homeless.csv")

Data_Indoor <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Indoor Air Quality'),]
Data_group <-Data_Indoor %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Indoor.csv")

Data_Mold <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Mold'),]
Data_group <-Data_Mold %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Mold.csv")


Data_Water <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Water Quality'),]
Data_group <-Data_Water %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Water.csv")

Data_Urin <- Data_fullzip[(Data_fullzip$Complaint.Type == 'Urinating in Public'),]
Data_group <-Data_Urin %>%
  group_by(zip, year_factor) 
sumMon <- summarize(Data_group, count = n())

write.csv(sumMon, file = "311Aggregate-Urin.csv")

sumMon$zip <- sapply(sumMon$zip, as.character)
sumMon$year_factor <- sapply(sumMon$year_factor, as.character)

sumMon_clean <- sumMon[(sumMon$zip != "0" & sumMon$zip != "11801" & sumMon$zip != "12345" & sumMon$zip != "N/A"& sumMon$zip != "NY" & sumMon$zip != "10000" & sumMon$zip != "10338" & sumMon$zip != "11571" ),]
sumMon_clean

#sumMon_clean$zip <- sapply(sumMon_clean$zip, as.numeric)
#sumMon_clean$year_factor <- sapply(sumMon_clean$year_factor, as.numeric)
sumMon_clean2 <- sumMon_clean
#sumMon_clean2$zip <- sapply(sumMon_clean$zip, as.integer)
#sumMon_clean2$year_factor <- sapply(sumMon_clean$year_factor, as.integer)
colnames(sumMon_clean2) <- c("zipcode", "year", "count")
sumMon_2004 <- sumMon_clean2[(sumMon_clean$year_factor == "2004"),]
sumMon_2005 <- sumMon_clean2[(sumMon_clean$year_factor == "2005"),]
sumMon_2006 <- sumMon_clean2[(sumMon_clean$year_factor == "2006"),]
sumMon_2007 <- sumMon_clean2[(sumMon_clean$year_factor == "2007"),]
sumMon_2008 <- sumMon_clean2[(sumMon_clean$year_factor == "2008"),]
sumMon_2009 <- sumMon_clean2[(sumMon_clean$year_factor == "2009"),]
sumMon_2010 <- sumMon_clean2[(sumMon_clean$year_factor == "2010"),]
sumMon_2011 <- sumMon_clean2[(sumMon_clean$year_factor == "2011"),]
sumMon_2012 <- sumMon_clean2[(sumMon_clean$year_factor == "2012"),]
sumMon_2013 <- sumMon_clean2[(sumMon_clean$year_factor == "2013"),]
sumMon_2014 <- sumMon_clean2[(sumMon_clean$year_factor == "2014"),]
sumMon_2015 <- sumMon_clean2[(sumMon_clean$year_factor == "2015"),]










################

data(df_zip_demographics)
colnames(sumMon_2004) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)

# print a map for each column of the demographic data.frame
sumMon_2004$value = sumMon_2004[,3]
title = "2004 New York City 311 Health Complaint Map"
  
  # print the map
choro = zip_choropleth(sumMon_2004, title=title, county_zoom=nyc_fips)
print(choro)n
######################
colnames(sumMon_2005) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2005 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2005, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2005) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2005 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2005, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2006) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2006 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2006, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2007) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2007 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2007, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2007) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2007 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2007, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2008) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2008 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2008, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2009) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2009 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2009, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2010) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2010 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2010, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2011) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2011 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2011, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2012) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2012 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2012, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2013) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2013 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2013, title=title, county_zoom=nyc_fips)
print(choro)

######################
colnames(sumMon_2014) = c("region", "year", "value")
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
title = "2014 New York City 311 Health Complain Map"
choro = zip_choropleth(sumMon_2014, title=title, county_zoom=nyc_fips)
print(choro)




                  
