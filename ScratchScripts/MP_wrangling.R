getwd()
#packages
require(tidyverse)
require(dplyr)
require(sf)
require(leaflet)
require(mapview)
require(readxl)
#make mapview work on a mac
mapviewOptions(fgb = FALSE)

#load data
records_2014 <- read.csv("./By_year/2014_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2015 <- read.csv("./By_year/2015_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2016 <- read.csv("./By_year/2016_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2017 <- read.csv("./By_year/2017_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2018 <- read.csv("./By_year/2018_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2019 <- read.csv("./By_year/2019_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2020 <- read.csv("./By_year/2020_Records_Yalbac_data.csv", stringsAsFactors = T)

#2016 need to add where the cameras were to the data
utm_2016 <- read_xlsx("./By_year/2016UTM.xlsx")

unique(records_2014$Species)
unique(records_2015$Species)
unique(records_2016$species.name)
unique(records_2017$species.name)
unique(records_2018$species.name)
unique(records_2019$species.name)
unique(records_2020$species.name)

#spatial feature manipulation
# can probs take out drop na and go back in to see camera name and add location
clean_2014.sf <- records_2014 %>%
  select(1:14) %>%
  drop_na(Easting, Northing) %>%
  st_as_sf(coords = c('Easting', 'Northing'),
         crs = 26716)

clean_2015.sf <- records_2015 %>%
  select(1:3,6:12,14,16:18) %>%
  drop_na(Easting.1, Northing.1) %>%
  st_as_sf(coords = c('Easting.1', 'Northing.1'),
           crs = 26716)

clean_2016.sf <- records_2016 %>%
  mutate(Name = paste0(Cam.nr,series)) %>%
  left_join(utm_2016, by = "Name") # %>%
 # drop_na(Easting, Northing) %>%
  #st_as_sf(coords = c('Easting', 'Northing'),
         #  crs = 26716)
unique(clean_2016.sf$Name)

clean_2018.sf <- records_2018 %>%
  select(2:14,17:18) %>%
  drop_na(Easting, Northing) %>%
  st_as_sf(coords = c('Easting', 'Northing'),
           crs = 26716)


clean_2019.sf <- records_2019 %>%
  select(1:17,19) %>%
  drop_na(Easting, Northing) %>%
  st_as_sf(coords = c('Easting','Northing'),
           crs=26716)

clean_2020.sf <- records_2020 %>%
  select(1:9,11:15,17:21) %>%
  drop_na(Easting, Northing) %>%
  st_as_sf(coords = c('Easting','Northing'),
           crs=26716)



mapview(clean_2014.sf, col.regions = "pink") +
mapview(clean_2015.sf, col.regions = "purple") +
mapview(clean_2018.sf, col.regions = "yellow") +
mapview(clean_2019.sf, zcol = "Logging", col.regions = c("blue", "pink")) +
mapview(clean_2020.sf, zcol = "Logging") 

mapview(clean_2014.sf, col.regions = "pink") +
  mapview(clean_2015.sf, col.regions = "purple") +
  mapview(clean_2018.sf, col.regions = "yellow") +
  mapview(clean_2019.sf, col.regions = "blue") +
  mapview(clean_2020.sf, col.regions = "green") 
