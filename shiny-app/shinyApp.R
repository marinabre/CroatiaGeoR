if (!require('shiny')) install.packages("shiny")
if (!require('shinydashboard')) install.packages("shinydashboard")
if (!require('leaflet')) install.packages("leaflet")
if (!require('rgdal')) install.packages("rgdal") 
if (!require('maps')) install.packages("maps")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('magrittr')) install.packages("magrittr") 
if (!require('RColorBrewer')) install.packages("RColorBrewer") 


source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

