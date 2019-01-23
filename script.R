#   ---   script.r  ---
#
#     January 2019
#
# ----------------------

# required packages 
install.packages('car')
install.packages('tidyverse')
install.packages('readr')
install.packages('dplyr')   
install.packages('fuzzyjoin')
install.packages('ggplot2')
install.packages('hrbrthemes', dependencies = TRUE)
hrbrthemes::import_roboto_condensed() 
install.packages('sf', dependencies = TRUE)
install.packages('mapview')
install.packages('leaflet')
install.packages('tmap')
install.packages('tmaptools')
install.packages('OpenStreetMap', dependencies = TRUE)
devtools::install_github("r-lib/httr")


#     SETTING UP 
# 
#
library(dplyr)    
library(tidyverse)
library(readr)
library(ggplot2)
library(fuzzyjoin)
library(hrbrthemes)
# sf primarily for vector data
library(sf)
library(mapview)
library(leaflet)
library(httr)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
suppressPackageStartupMessages(library(tidyverse))

#  import datasets
# 
# 2015
eduper <- read_csv("./data/OECD_StudentExpenditure-edited.csv")

view(eduper)

