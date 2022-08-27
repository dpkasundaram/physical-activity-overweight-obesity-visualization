library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork) 
library(hrbrthemes)
library(lubridate)
library(zoo)
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcodeR)
library(leaflet)
library(gridExtra)
#library(choroplethrZip)
library(devtools)
library(treemapify)
library(pacman)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(DT)
library(sf)
library(ggthemes)
library(gganimate)
library(usmap)
library(DT)
library(shiny)
library(tidyverse)
library(bslib)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(tableHTML)
library(leaflet)
library(plotly)
library(gifski)
library(png)


readingdata <- function() {

out <- list()
df_raw <- read_csv("www/Data/Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")


#removing unwanted and duplicate columns
out$df_raw <- df_raw[, -c(2,5,7,9,10,12,13,14,17,18,25,26,27,28,29,32,33)]

#removing records for National category as we will analyse state wise
#df_raw <- df_raw[!(df_raw$LocationDesc == "National"),]

out$df_Nutrition <- df_raw[df_raw$Class == "Fruits and Vegetables",]
out$df_Obesity <- df_raw[df_raw$Class == "Obesity / Weight Status",]
out$df_Physical <- df_raw[df_raw$Class == "Physical Activity",]

out$df_whole <- df_raw[!df_raw$Class == "Fruits and Vegetables",]

out$df_ExPattern <- out$df_Physical %>%
  mutate(Exercise_Pattern = case_when(
    Question == "Percent of adults who engage in no leisure-time physical activity" ~ "Inactive",
    Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ "Less Active",
    Question == "Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ "Highly Active",
    Question == "Percent of adults who engage in muscle-strengthening activities on 2 or more days a week" ~ "Moderately Active",
    Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week" ~ "Extremely Active"))




return(out)
}
