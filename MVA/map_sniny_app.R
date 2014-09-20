

# This is my first shiny app:
dir <- "C:/Users/Alejandra/Dropbox/DSSG/"

setwd(dir)

install.packages("shiny")
library(shiny)


install.packages(c("maps", "mapproj"))
library(maps)
library(mapproj)
source("map_app/helpers.R")

# -------------------------------------------------------------------------------------------------------- #

counties <- readRDS("map_app/data/counties.rds")
head(counties)
percent_map(counties$white, "darkgreen", "% white")

# -------------------------------------------------------------------------------------------------------- #

runApp("map_app")
