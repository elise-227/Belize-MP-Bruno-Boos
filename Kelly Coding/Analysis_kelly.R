#clean space
#load packages
#packages
require(tidyverse)
require(dplyr)
require(sf)
require(leaflet)
require(mapview)
require(readxl)
require(lubridate)
require(readxl)
library(unmarked)
library(AICcmodavg)
library(camtrapR)
library(ubms)
#set wd

##load dfs
load("obs_data.RData")
load("cam_op.RData")

#make camera_operation frame