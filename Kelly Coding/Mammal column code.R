
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
records_2015 <- read.csv("./By_year/2015_Records_Yalbac_data.csv", stringsAsFactors = T)


#collect species names
names2014 <- unique(records_2014$Species)
names2015 <- unique(records_2015$Species)

#save a string with ""
a <- paste0('"', paste(names2014, collapse='", "'), '"')
print(a, quote=F)
b <- paste0('"', paste(names2015, collapse='", "'), '"')
print(b, quote=F)

##now I have to google all of these species lol
##then code as mammal Y or N
#2014
#fix errors
records_2014$Species <- gsub("Squirel", "Squirrel", records_2014$Species)
records_2014$Species <- gsub("Black-faced Anthrush", "Black-faced Antthrush", records_2014$Species)
records_2014$Species <- gsub("Clay coloured Thrush", "Clay-coloured Thrush", records_2014$Species)
records_2014$Species <- gsub("Four-eyed Oppossum", "Four-eyed Opossum", records_2014$Species)
records_2014$Species <- gsub("Ruddy-quail Dove", "Ruddy Quail-dove", records_2014$Species)
records_2014$Species <- gsub("Stripe-nosed Skunk", "Striped Hog-nosed Skunk", records_2014$Species)

#add mammal Y/N
records_2014$Mammal <- ifelse(records_2014$Species %in% c("Agouti", "Central American Tapir", "Collared Peccary", "Common Opossum", 
                                                          "Deppe's Squirrel", "Four-eyed Opossum", "Gray Fox", "Jaguar", "Jaguarundi", 
                                                          "Margay", "Mouse", "Nine-banded Armadillo", "Northern Tamandua", "Ocelot", "Paca", 
                                                          "Puma", "Racoon", "Rat", "Red Brocket Deer", "Rodent", "Squirrel", "Striped Hog-nosed Skunk", 
                                                          "Tayra", "White-lipped Peccary", "White-nosed Coati", "White-tailed Deer"), "Yes",
                       ifelse(records_2014$Species %in% c("0", "Ameiva undata", "Bare-throated Tiger Heron", "Black-faced Antthrush", "Blue-crowned Mot-mot", 
                                                 "Clay-coloured Thrush", "Collared Forest Falcon", "Common Pauraque", 
                                                 "Crested Guan", "White-tipped Dove", "Gray-headed Dove", "Gray-necked Woodrail", "Great Black Hawk", 
                                                 "Great Curassow", "Great Tinamou", "INDENT Dove", "INDENT Mammal", "INDENT Owl", "INDENT Pigeon", 
                                                 "Ocellated Turkey", "Ovenbird", "Plain Chachalaca", "Red-billed Pigeon", "Ruddy Quail-dove", 
                                                 "Slaty-breasted Tinamou", "Swainson's Trush", "Tinamou", "UNID", "White Hawk", "White-collard Seedeater", 
                                                 "Wood Thrush"), "No", NA))

#2015
#fix misspelled species
records_2015$Species <- gsub("Collared Pecary", "Collared Peccary", records_2015$Species)
records_2015$Species <- gsub("Gray-necked woodrail ", "Gray-necked Woodrail", records_2015$Species)
records_2015$Species <- gsub("Great Black-Hawk", "Great Black Hawk", records_2015$Species)
records_2015$Species <- gsub("Louisiana waterthrush", "Louisiana Waterthrush", records_2015$Species)
records_2015$Species <- gsub("Ruddy-Quaildove", "Ruddy Quail-dove", records_2015$Species)
records_2015$Species <- gsub("Squirrel Cucko", "Squirrel Cuckoo", records_2015$Species)
records_2015$Species <- gsub("Squirrel Cuckooo", "Squirrel Cuckoo", records_2015$Species)
records_2015$Species <- gsub("Yucatan Squirel", "Yucatan Squirrel", records_2015$Species)

#add mammal Y/N
records_2015$Mammal <- ifelse(records_2015$Species %in% c("Agouti", "Collared Peccary", "Deppe's Squirrel", "Gray Fox", 
                                                          "Jaguar", "Margay", "Mouse", "Nine-banded Armadillo", 
                                                          "Northern Tamandua", "Ocelot", "Opossum", "Paca", 
                                                          "Puma", "Racoon", "Red Brocket Deer", "Striped Hog-nosed Skunk",
                                                          "Stripe-throated Hermit", "Swainson's Thrush", "Tapir", "Tayra", 
                                                          "White-lipped Peccary", "White-nosed Coati", "White-tailed Deer", 
                                                          "Yucatan Squirrel"), "Yes",
                              ifelse(records_2015$Species %in% c("Bare-throated Tiger Heron", "Birds", "Black-faced Ant Thrush", 
                                                                                        "Clay coloured Thrush", "Blue Ground Dove", "Crested Guan", 
                                                                                        "Gray-headed Dove", "Gray-headed Kite", "Gray-necked Woodrail",
                                                                                        "Great Black Hawk", "Great Curassow", "Great Tinamou", "Green-backed Sparrow", 
                                                                                        "Hawk", "Louisiana Waterthrush", "Northern Waterthrush", "Ocellated Turkey", 
                                                                                        "Ornate Hawk-Eagle", "Pauraque", "Plain Chachalaca", "Ruddy Quail-dove",  
                                                                                        "Slaty-breasted Tinamou", "Squirrel Cuckoo", "Thicket Tinamou", "UNID Tinamou", 
                                                                                        "White-tipped Dove", "Wood Thrush", ""), "No", NA))
##check if any NAs
unique(records_2014$Mammal)
unique(records_2015$Mammal)

##check distribution of mammals
dist_2014 <- records_2014 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
dist_2015 <- records_2015 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))


print(dist_2014)
print(dist_2015)
