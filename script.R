#   ---   script.r  ---
#
#     January 2019 Datathon
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
install.packages('stringr')


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
library(stringr)
suppressPackageStartupMessages(library(tidyverse))

#  IMPORTING DATA SETS
# 
#
# expenditure as $ GDP (OECD)
eduper <- read_csv("./data/OECD_StudentExpenditure-edited.csv")
view(eduper)

# creating plot 
eduper %>% 
  select(`Country`, `All expenditure types as GDP percentage`) %>% 
  View

# % of gdp spending on all educational expenditures in OECD countries  
eduper %>% 
  drop_na() %>% 
  select(`Country`, `All expenditure types as GDP percentage`) %>% 
  mutate(country = `Country`) %>% 
  mutate(gdp = fct_reorder(`All expenditure types as GDP percentage`)) %>% 
  filter(!(gdp == '..' ) )%>% 
  arrange(desc(gdp)) %>% 
  mutate(colour = if_else(gdp > 1.4, "white", "black")) %>% 
  #group_by(`Country`, `All expenditure types as GDP percentage`) %>% 
  ggplot(aes(y=gdp, x=country, width = 0.8)) + 
  #adding layer, a bar chart
  geom_bar(stat = "identity", fill = '#086375') +
  coord_flip() +
  geom_text(aes(label = country, y = gdp, colour = colour), hjust = "inward", 
            vjust = "center", size = 2) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  scale_x_discrete(labels = NULL) +
  xlab("") +
  ylab("") +
  theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  theme(plot.title = element_text(size = 12)) + 
  labs(title = "Education expenditure in OECD countries as % of GDP", caption = "Source: OECD, computation by Sciences Po students.")





