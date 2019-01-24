# --------------- script.r -----------------------
#
#     January 2019 Datathon
#   
#     Gloriana Lang, India Hayler Kerle, Francesco Lanzone, 
#     Maximilian Gahntz, and Stephanie Tran
# 
# --------------------------------------------------

# required packages (run once)
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
install.packages('stringr')
install.packages('sqldf')
install.packages('xlsx')
install.packages("maps")
install.packages("mapproj")
install.packages('geojsonio')
install.packages('geojsonlint')
install.packages('broom')
install.packages('jsonlite')

# ----------------------------
#    --- SETTING UP ---
# 
#----------------------------
library(dplyr)    
library(tidyverse)
library(readr)
library(ggplot2)
library(fuzzyjoin)
library(hrbrthemes)
library(sf)
library(mapview)
library(leaflet)
library(httr)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(stringr)
suppressPackageStartupMessages(library(tidyverse))
library(maps)
library(mapproj)
library(geojsonio)
library(geojsonlint)
library(jsonlite)
library(broom)

#----------------------------------------
#     ---   IMPORTING DATA SETS  ---
# 
# ----------------------------------------

#   //  object: eduper    
#   //  expenditure as $ GDP (OECD)
eduOecd <- read_csv("./data/OECD_StudentExpenditure-edited.csv")
eduOecd <- na.omit(eduOecd)
eduOecd <- select(eduOecd,'country', 'allGdp')
# export data frame of % GDP as file
write.xlsx(eduOecd, "./data/edu-oecd.xlsx")
view(eduOecd)

#   //  temp object: eduregio (for edu)   
#   //  Effectifs d'étudiants inscrits 2014-15
eduregio <- read_delim("./data/Enrollment.csv", delim = ";", locale = locale(encoding = "Latin1"))
studnum <- eduregio %>% 
  group_by(Région) %>%
  summarise(studnum = sum(as.numeric(`Effectifs d'étudiants inscrits 2014-15`), na.rm = TRUE))

#   //  temp object: eduexp (for edu)
#   //  spending data
eduexp <- read_delim("./data/SpendingData.csv", delim = ";", locale = locale(encoding = "Latin1"))
# removing unwanted columns
studnum <- studnum[-c(5, 7),]
studnum <- studnum[-c(7, 8, 11, 12),]
eduexp <- eduexp[-c(14:70),]

# changing col name to match each other
eduexp$Region[13] <- "Provence-Alpes-Côte d'Azur"
colnames(eduexp) <- c("Région", "Total Spending", "Regional Councils", "Departmental Councils", "Communes and EPCI", "X6")

#   //  object: edu    
#   //  Total spending per student 
edu <- left_join(eduexp, studnum, by = NULL)
edu$X6 <- NULL
rm(eduregio, eduexp, studnum)

# creating spending per student column
edu$SpendingPerStudent <- (edu$`Total Spending`*1000000/edu$studnum)
# view(edu$SpendingPerStudent)
view(edu)
# export .csv file of data frame
# write.csv(edu, file = "./data/spending-per-capita.csv")


#
# --  merge spending per capita table with geographic location
#

#   //  object: regionsAndSpending
#   //  spatial data on regions of France and create map
regdata <- fromJSON("./data/map.geojson")
# view region names from nested json file
str(regdata$features$geometry)
# create list of geometry points from json data
points <- (regdata$features$geometry)

# create dataframe with region names and geomtry
regions <- data.frame("nom" = c(regdata$features$properties$nom),
                      "location" = points )
view(regions)
# merge coordinates into edu object
regionsAndSpending <- left_join(edu, regions, by = c("Région" = "nom"))
view(regionsAndSpending)

class(regionsAndSpending$location.coordinates)

mpol <- st_multipolygon(regionsAndSpending$location.coordinates)
view(regionsAndSpending)

regionsAndSpending

# !!!! TO DO: map 


# -------------------------------------------
#    ----    PLOT MAKING     ----   
#
# --------------------------------------------

# mode function (via https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# % of gdp spending on all educational expenditures in OECD countries  
eduMean <- mean(eduOecd$allGdp)
eduMedian <- median(eduOecd$allGdp)
eduMode <- getmode(eduOecd$allGdp)
eduMean
eduOecd <- eduOecd %>% 
  drop_na() %>% 
  select(`country`, `allGdp`) %>% 
  mutate(gdp = `allGdp`) 
  
view(eduOecd)

eduOecd %>% 
  mutate(highlighted= ifelse(`country` == 'France', T, F))  %>% 
  # set var for France to be highlightd
  #mutate( ToHighlight = ifelse( cyl == 6, "yes", "no" ) ) %%
  arrange(desc(gdp)) %>% 
#  mutate(colour = if_else(gdp > 1, "white", "black")) %>% 
  #group_by(`Country`, `All expenditure types as GDP percentage`) %>% 
  ggplot(aes(x = reorder(`country`, gdp), y = gdp)) + 
  #adding layer, a bar chart
  geom_bar(stat = "identity", aes(fill = highlighted)) +
  # add line indicating average
  geom_hline(aes(yintercept = mean(gdp)),col='orange',size=1, alpha=0.4) +
  geom_text(aes(0,mean(gdp)),label = 'Average: 1.5%', vjust = -1, hjust=0, nudge_x = 2, nudge_y=0.03) + 
  coord_flip() +
  geom_text(aes(label = country, y = gdp, colour = 'white'), colour='white', hjust = 1, 
            vjust = "center", size = 2.8, nudge_y = -0.03) +
  scale_fill_manual(values = c('#218380','#D81159') , guide = FALSE) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks=seq(0,2.6,0.2)) +
  xlab("") +
  ylab("") +
  # theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  theme(plot.title = element_text(size = 12)) +
  labs(title = "Education expenditure in OECD countries as % of GDP (2015)", caption = "Source: OECD, computation by Sciences Po students.")

mpol <- st_multipolygon(regionsAndSpending$location.coordinates)
view(regionsAndSpending)


regionMean <- mean(regionsAndSpending$SpendingPerStudent)
regionMedian <- median(regionsAndSpending$SpendingPerStudent)

regionMean

regionsAndSpending %>% 
  ggplot(aes(x = reorder(`Région`, `SpendingPerStudent`), y = `SpendingPerStudent`)) +
  #adding layer, a bar chart
  geom_bar(stat = "identity", aes(fill = "purple"))+
 # geom_text(aes(0,mean(gdp)),label = 'Average: 1.5%', vjust = -1, hjust=0, nudge_x = 2, nudge_y=0.03) + 
  #geom_text(aes(label = country, y = gdp, colour = 'white'), colour='white', hjust = 1, 
  #          vjust = "center", size = 2.8, nudge_y = -0.03) +
  # theme(axis.ticks.y = element_blank()) +
  coord_flip() + 
  scale_y_continuous(breaks=seq(0, 700, by = 50)) +
  xlab("") +
  ylab("") +
  # add line indicating average
  geom_hline(aes(yintercept = mean(`SpendingPerStudent`)),col='orange',size=1, alpha=0.4) +
  geom_text(aes(label = sprintf("%0.2f", round(`SpendingPerStudent`, digits = 2)), y = `SpendingPerStudent`, colour = 'white'), colour='white', hjust = 1, 
            vjust = "center", size = 3.0, nudge_y = -4) +
 annotate("text", x = 4, y = 380, label = "Average: 318.32", family="Roboto Condensed") + 
  theme_ipsum(grid = "X") +
  scale_fill_manual(values = c('#218380','#D81159') , guide = FALSE) +
  theme(plot.title = element_text(size = 12)) +
  labs(title = "Spending per student in each region in France", caption = "Source: Ministère de l'Éducation nationale et de la Jeunesse, computation by Sciences Po students.")


