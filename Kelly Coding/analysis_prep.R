###Prepare master frames###
dat2014 <- read.csv("data2014_final.csv")
dat2015 <- read.csv("data2015_final.csv")
#dat2016 <- read.csv("data2016_final.csv") #missing
dat2017 <- read.csv("data2017_final.csv")
dat2018 <- read.csv("data2018_final.csv")
#dat2019 <- read.csv("data2019_final.csv") #missing
#dat2020 <- read.csv("data2020_final.csv") #missing

#add mammal column to 2017
#fix errors
dat2017$Species <- gsub("Gray fox", "Gray Fox", dat2017$Species)
dat2017$Species <- gsub("Ruddy Quail Dove ", "Ruddy Quail-dove", dat2017$Species)
dat2017$Species <- gsub("Collared Pecary", "Collared Peccary", dat2017$Species)
dat2017$Species <- gsub("Crested guan", "Crested Guan", dat2017$Species)
dat2017$Species <- gsub("Lesson's motmot", "Lesson's Motmot", dat2017$Species)
dat2017$Species <- gsub("Bare-throated Tiger-heron", "Bare-throated Tiger Heron", dat2017$Species)
dat2017$Species <- gsub("Black-crowned Night-Heron", "Black-crowned Night Heron", dat2017$Species)
dat2017$Species <- gsub("Great Blue-heron", "Great Blue Heron", dat2017$Species)
dat2017$Species <- gsub("Clay coloured Thrush", "Clay-coloured Thrush", dat2017$Species)
dat2017$Species <- gsub("Yellow-crowned Night-Heron", "Yellow-crowned Night Heron", dat2017$Species)
dat2017$Species <- gsub("Blue Ground-Dove", "Blue Ground Dove", dat2017$Species)
dat2017$Species <- gsub("Gray catbird", "Gray Catbird", dat2017$Species)

#add mammal Y/N
dat2017$Mammal <- ifelse(dat2017$Species %in% c("Baird's Tapir", "Puma", "Red Brocket Deer", "White-lipped Peccary", 
                                                               "White-tailed Deer", "Ocelot", "Agouti", "Gray Fox", 
                                                               "Tamandua", "White-nosed Coati", "Common Opossum", "Jaguar", 
                                                               "Margay", "Paca", "Tayra", "Racoon", "Striped Hog-nosed Skunk", 
                                                               "Otter", "Brown Four-eyed Opossum", "Nine-banded Armadillo", 
                                                               "Coyote", "Jaguarundi", "Rodent", "Social Flycatcher", "Squirrel", 
                                                               "Deppe's Squirrel"), "Yes",
                              ifelse(dat2017$Species %in% c("Great Curassow", "Ocellated Turkey", "Unknown", "Humans", 
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



#align columns
dat2014_2 <- dat2014[,c("site", "Location", "Easting", "Northing", "obsdate", "Last_Record_clean", "Days", "date_time_obs", "Species", "Mammal", "Year", "Month", "Day", "Hour")]
dat2015_2 <- dat2015[,c("site", "Location", "Easting", "Northing", "obsdate", "Last_Record_clean", "Days", "date_time_obs", "Species", "Mammal", "Year", "Month", "Day", "Hour")]
dat2017_2 <- dat2017[,c("site", "Location", "Easting", "Northing", "obsdate", "Last_Record_clean", "Days", "date_time_obs", "Species", "Mammal", "Year", "Month", "Day", "Hour")]
dat2018_2 <- dat2018[,c("site", "Location", "Easting", "Northing", "obsdate", "Last_Record_clean", "Days", "date_time_obs", "Species", "Mammal", "Year", "Month", "Day", "Hour")]

#bind columns together
dat <- rbind(dat2014_2, dat2015_2, dat2017_2, dat2018_2)


#load cam_ops files
camop_2014 <- read.csv("camop_2014.csv")
camop_2015 <- read.csv("camop_2015.csv")
camop_2017 <- read.csv("camop_2017.csv")
camop_2018 <- read.csv("camop_2018.csv")

#bind cam_ops
cam_op <- rbind(camop_2014,camop_2015,camop_2017,camop_2018)

#save to source
save(dat, file = "obs_data.RData")
save(cam_op, file = "cam_op.RData")
