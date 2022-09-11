
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
records_2016 <- read.csv("./By_year/2016_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2017 <- read.csv("./By_year/2017_Records_Yalbac_data.csv", stringsAsFactors = T)
records_2018 <- read.csv("./By_year/2018_Records_Yalbac_data.csv", stringsAsFactors = T)


#collect species names
names2014 <- unique(records_2014$Species)
names2015 <- unique(records_2015$Species)
names2016 <- unique(records_2016$species.name)
names2017 <- unique(records_2017$species.name)
names2018 <- unique(records_2018$species.name)


#save a string with ""
a <- paste0('"', paste(names2014, collapse='", "'), '"') #2014
print(a, quote=F)
b <- paste0('"', paste(names2015, collapse='", "'), '"') #2015
print(b, quote=F)
c <- paste0('"', paste(names2016, collapse='", "'), '"') #2016
print(c, quote=F)
d <- paste0('"', paste(names2017, collapse='", "'), '"') #2017
print(d, quote=F)
e <- paste0('"', paste(names2018, collapse='", "'), '"') #2018
print(e, quote=F)

##now I have to google all of these species lol
##then code as mammal Y or N
################################################################################
##2014##########################################################################
################################################################################
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

################################################################################
##2015##########################################################################
################################################################################
#fix misspelled species
records_2015$Species <- gsub("Collared Pecary", "Collared Peccary", records_2015$Species)
records_2015$Species <- gsub("Gray-necked woodrail ", "Gray-necked Woodrail", records_2015$Species)
records_2015$Species <- gsub("Great Black-Hawk", "Great Black Hawk", records_2015$Species)
records_2015$Species <- gsub("Louisiana waterthrush", "Louisiana Waterthrush", records_2015$Species)
records_2015$Species <- gsub("Ruddy-Quaildove", "Ruddy Quail-dove", records_2015$Species)
records_2015$Species <- gsub("Squirrel Cucko", "Squirrel Cuckoo", records_2015$Species)
records_2015$Species <- gsub("Squirrel Cuckooo", "Squirrel Cuckoo", records_2015$Species)
records_2015$Species <- gsub("Yucatan Squirel", "Yucatan Squirrel", records_2015$Species)
records_2015$Species <- gsub("Ornate Hawk-Eagle", "Ornate Hawk-eagle", records_2015$Species)

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
                                                                                        "Ornate Hawk-eagle", "Pauraque", "Plain Chachalaca", "Ruddy Quail-dove",  
                                                                                        "Slaty-breasted Tinamou", "Squirrel Cuckoo", "Thicket Tinamou", "UNID Tinamou", 
                                                                                        "White-tipped Dove", "Wood Thrush", ""), "No", NA))
################################################################################
##2016##########################################################################
################################################################################
#fix misspelled species
records_2016$species.name <- gsub("Gray fox", "Gray Fox", records_2016$species.name)
records_2016$species.name <- gsub("Collared Pecary", "Collared Peccary", records_2016$species.name)
records_2016$species.name <- gsub("Crested guan", "Crested Guan", records_2016$species.name)
records_2016$species.name <- gsub("Clay coloured Thrush", "Clay-coloured Thrush", records_2016$species.name)
records_2016$species.name <- gsub("Commn Opossum", "Common Opossum", records_2016$species.name)
records_2016$species.name <- gsub("Gray hawk", "Gray Hawk", records_2016$species.name)
records_2016$species.name <- gsub("Rat and dragon fly", "Rat", records_2016$species.name)
records_2016$species.name <- gsub("Ruddy Quail Dove ", "Ruddy Quail-dove", records_2016$species.name)
records_2016$species.name <- gsub("Black hawk eagle", "Black Hawk-eagle", records_2016$species.name)
records_2016$species.name <- gsub("Roadside hawk", "Roadside Hawk", records_2016$species.name)
records_2016$species.name <- gsub("Eastern/Tropical pewee", "Pewee", records_2016$species.name)
records_2016$species.name <- gsub("agouti", "Agouti", records_2016$species.name)
records_2016$species.name <- gsub("agouti ", "Agouti", records_2016$species.name)
records_2016$species.name <- gsub("Agouti ", "Agouti", records_2016$species.name)
records_2016$species.name <- gsub("ocelot", "Ocelot", records_2016$species.name)

#add mammal Y/N
records_2016$Mammal <- ifelse(records_2016$species.name %in% c("Agouti", "Gray Fox", "Margay", "Ocelot", "Puma", "Spider Monkey", 
                                                          "Tayra", "White-nosed Coati", "White-tailed Deer", "Collared Peccary", 
                                                          "Paca", "Red Brocket Deer", "Jaguar", "Common Opossum", "Rat", 
                                                          "Baird's Tapir", "White-lipped Peccary", "Striped Hog-nosed Skunk", "Racoon", 
                                                          "Brown Four-eyed Opossum", "Nine-banded Armadillo", "Tamandua", 
                                                          "Ocelot", "Rodent"), "Yes",
                              ifelse(records_2016$species.name %in% c("Great Curassow", "unknown", "Ocellated Turkey", "Crested Guan", 
                                                                 "Chachalaka", "Clay-coloured Thrush", "Dragon fly", "Flycatcher", 
                                                                 "Gray Hawk", "Gray-headed Dove", "Great Tinamou", "lizard", 
                                                                 "Ruddy Quail-dove", "Scaled Pigeon", "White-tipped Dove", 
                                                                 "Slaty-breasted Tinamou", "Wood Thrush", "Great Black Hawk", 
                                                                 "Limpkin", "Northern Waterthrush", "Black Hawk-eagle",
                                                                 "Blue Ground Dove", "Roadside Hawk", "Pewee", "Boat-billed Heron", 
                                                                 "Little Tinamou", ""), "No", NA))

################################################################################
##2017##########################################################################
################################################################################
#fix errors
records_2017$species.name <- gsub("Gray fox", "Gray Fox", records_2017$species.name)
records_2017$species.name <- gsub("Ruddy Quail Dove ", "Ruddy Quail-dove", records_2017$species.name)
records_2017$species.name <- gsub("Collared Pecary", "Collared Peccary", records_2017$species.name)
records_2017$species.name <- gsub("Crested guan", "Crested Guan", records_2017$species.name)
records_2017$species.name <- gsub("Lesson's motmot", "Lesson's Motmot", records_2017$species.name)
records_2017$species.name <- gsub("Bare-throated Tiger-heron", "Bare-throated Tiger Heron", records_2017$species.name)
records_2017$species.name <- gsub("Black-crowned Night-Heron", "Black-crowned Night Heron", records_2017$species.name)
records_2017$species.name <- gsub("Great Blue-heron", "Great Blue Heron", records_2017$species.name)
records_2017$species.name <- gsub("Clay coloured Thrush", "Clay-coloured Thrush", records_2017$species.name)
records_2017$species.name <- gsub("Yellow-crowned Night-Heron", "Yellow-crowned Night Heron", records_2017$species.name)
records_2017$species.name <- gsub("Blue Ground-Dove", "Blue Ground Dove", records_2017$species.name)
records_2017$species.name <- gsub("Gray catbird", "Gray Catbird", records_2017$species.name)

#add mammal Y/N
records_2017$Mammal <- ifelse(records_2017$species.name %in% c("Baird's Tapir", "Puma", "Red Brocket Deer", "White-lipped Peccary", 
                                                               "White-tailed Deer", "Ocelot", "Agouti", "Gray Fox", 
                                                               "Tamandua", "White-nosed Coati", "Common Opossum", "Jaguar", 
                                                               "Margay", "Paca", "Tayra", "Racoon", "Striped Hog-nosed Skunk", 
                                                               "Otter", "Brown Four-eyed Opossum", "Nine-banded Armadillo", 
                                                               "Coyote", "Jaguarundi", "Rodent", "Social Flycatcher", "Squirrel", 
                                                               "Deppe's Squirrel"), "Yes",
                              ifelse(records_2017$species.name %in% c("Great Curassow", "Ocellated Turkey", "Unknown", "Humans", 
                                                                      "Mottled Owl", "Neeltje", "Gray-headed Dove", "Chachalaca", 
                                                                      "Ruddy Quail-dove", "Black-faced Antthrush", "Scaled Pigeon", 
                                                                      "Collared Peccary", "Lesson's Motmot", "Wood Thrush", 
                                                                      "Boat-billed Heron", "Slaty-breasted Tinamou", "Bare-throated Tiger Heron", 
                                                                      "Black-crowned Night Heron", "Cane Toad", "Great Blue Heron", 
                                                                      "Great Tinamou", "Limpkin", "Agami Heron", "Clay-coloured Thrush", 
                                                                      "Yellow-crowned Night Heron", "Russet-naped Woodrail", "Bird", 
                                                                      "Blue Ground Dove", "Gray Catbird", "Gray-chested Dove", "Hawk", 
                                                                      "Northern Waterthrush", "Ornate Hawk-eagle", "Roadside Hawk", 
                                                                      "Little Tinamou", "Crested Guan"), "No", NA))

################################################################################
##2018##########################################################################
################################################################################
#fix errors
records_2018$species.name <- gsub("Clay-colored Thrush", "Clay-coloured Thrush", records_2018$species.name)
records_2018$species.name <- gsub("Collared Pecary", "Collared Peccary", records_2018$species.name)
records_2018$species.name <- gsub("Crested guan", "Crested Guan", records_2018$species.name)
records_2018$species.name <- gsub("Gray fox", "Gray Fox", records_2018$species.name)
records_2018$species.name <- gsub("jaguar", "Jaguar", records_2018$species.name)
records_2018$species.name <- gsub("margay", "Margay", records_2018$species.name)
records_2018$species.name <- gsub("Ocellated turkey", "Ocellated Turkey", records_2018$species.name)
records_2018$species.name <- gsub("puma", "Puma", records_2018$species.name)
records_2018$species.name <- gsub("Ruddy Quail Dove ", "Ruddy Quail-dove", records_2018$species.name)
records_2018$species.name <- gsub("Striped hog-nosed skunk", "Striped Hog-nosed Skunk", records_2018$species.name)
records_2018$species.name <- gsub("tapir", "Tapir", records_2018$species.name)
records_2018$species.name <- gsub("tayra", "Tayra", records_2018$species.name)
records_2018$species.name <- gsub("Virginia opossum", "Virginia Opossum", records_2018$species.name)
records_2018$species.name <- gsub("Yellow-crowned Night-heron", "Yellow-crowned Night Heron", records_2018$species.name)
records_2018$species.name <- gsub("Bare-throated Tiger-heron", "Bare-throated Tiger Heron", records_2018$species.name)
records_2018$species.name <- gsub("Hooded warbler", "Hooded Warbler", records_2018$species.name)

#add mammal Y/N
records_2018$Mammal <- ifelse(records_2018$species.name %in% c("Agouti", "Baird's Tapir", "Collared Peccary", "Common Opossum", 
                                                               "Gray Four-eyed Opossum", "Gray Fox", "Jaguar", "Jaguarundi", 
                                                               "Margay", "Nine-banded Armadillo", "Northern Raccoon", "Ocelot", "Paca", 
                                                               "Puma", "Red Brocket Deer", "Striped Hog-nosed Skunk", "Tamandua", 
                                                               "Tapir", "Tayra", "Unidentified Mammal", "Unknown Rodent", "Virginia Opossum",
                                                               "White-nosed Coati", "White-tailed Deer", "Yucatan Squirrel", "Unidentified mammal", 
                                                               "White-lipped Peccary"), "Yes",
                              ifelse(records_2018$species.name %in% c("Chachalaca", "Clay-coloured Thrush", "Crested Guan", "Gray-headed Dove", 
                                                                      "Great Black Hawk", "Great Curassow", "Great Tinamou", "Lineated Woodpecker", 
                                                                      "Mottled Owl", "Ocellated Turkey", "Pauraque", "Ruddy Quail-dove", 
                                                                      "Russet-naped Woodrail", "Slaty-breasted Tinamou", "Tinamou sp.", "Unidentified ", 
                                                                      "Wood Thrush", "Yellow-crowned Night Heron", "Bare-throated Tiger Heron", 
                                                                      "Hooded Warbler", "Nothern Waterthrush", "Red-throated Ant-tanager", 
                                                                      "Ruddy Ground-dove", "Summer Tanager", "Thicket Tinamou", "Unknown", "Unknown bird", 
                                                                      ""), "No", NA))


##check if any NAs
unique(records_2014$Mammal)
unique(records_2015$Mammal)
unique(records_2016$Mammal)
unique(records_2017$Mammal)
unique(records_2018$Mammal)

##check distribution of mammals
dist_2014 <- records_2014 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
dist_2015 <- records_2015 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
dist_2016 <- records_2016 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
dist_2017 <- records_2017 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
dist_2018 <- records_2018 %>% group_by(Mammal) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))


print(dist_2014)
print(dist_2015)
print(dist_2016)
print(dist_2017)
print(dist_2018)
