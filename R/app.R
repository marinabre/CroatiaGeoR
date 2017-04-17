if (!require('shiny')) install.packages("shiny")
if (!require('shinydashboard')) install.packages("shinydashboard")

#if (!require('rsconnect'))install.packages('rsconnect')
#deploying
#library(rsconnect)
#rsconnect::deployApp('C:/Users/Ivyclover/Dropbox/DIPLOMSKI/R skripte/shiny-app')

source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

