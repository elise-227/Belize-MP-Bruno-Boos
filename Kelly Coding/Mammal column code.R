getwd()
#packages
require(tidyverse)
require(dplyr)
require(sf)
require(leaflet)
require(mapview)
require(readxl)
require(lubridate)

library(unmarked)
library(AICcmodavg)

#load data
records_2014 <- read.csv("./By_year/2014_Records_Yalbac_data.csv", stringsAsFactors = T)

#collect species names
names <- unique(records_2014$Species)
#save a string with ""
s <- paste0('"', paste(names, collapse='", "'), '"')
print(s, quote=F)

##now I have to google all of these species lol
##then code as mammal Y or N
records_2014$Mammal <- ifelse(records_2014$Species %in% c("Agouti", "Central American Tapir", "Collared Peccary", "Common Opossum", 
                                                          "Deppe's Squirrel", "Four-eyed Oppossum", "Gray Fox", "Jaguar", "Jaguarundi", 
                                                          "Margay", "Mouse", "Nine-banded Armadillo", "Northern Tamandua", "Ocelot", "Paca", 
                                                          "Puma", "Racoon", "Rat", "Red Brocket Deer", "Rodent", "Squirel", "Stripe-nosed Skunk", 
                                                          "Tayra", "White-lipped Peccary", "White-nosed Coati", "White-tailed Deer"), "Yes",
                       ifelse(records_2014$Species %in% c("0", "Ameiva undata", "Bare-throated Tiger Heron", "Black-faced Anthrush", "Blue-crowned Mot-mot", 
                                                 "Clay coloured Thrush", "Clay-coloured Thrush", "Collared Forest Falcon", "Common Pauraque", 
                                                 "Crested Guan", "White-tipped Dove", "Gray-headed Dove", "Gray-necked Woodrail", "Great Black Hawk", 
                                                 "Great Curassow", "Great Tinamou", "INDENT Dove", "INDENT Mammal", "INDENT Owl", "INDENT Pigeon", 
                                                 "Ocellated Turkey", "Ovenbird", "Plain Chachalaca", "Red-billed Pigeon", "Ruddy-quail Dove", 
                                                 "Slaty-breasted Tinamou", "Swainson's Trush", "Tinamou", "UNID", "White Hawk", "White-collard Seedeater", 
                                                 "Wood Thrush"), "No", NA))
##check if any NAs
unique(records_2014$Mammal)

##check distribution of mammals
mam <- records_2014 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
print(mam)