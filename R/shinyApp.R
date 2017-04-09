# install.packages("shiny")
# library(shiny)
# install.packages("shinydashboard")
# library(shinydashboard)


## source("abc.R")
## source("xyz.R")
## referenciranje druge .R skripte

#install.packages("ggmap")
#library(ggmap)
# cro_center <- c(16.542109374999992, 44.292107683334294)#as.numeric(geocode("Croatia"))
# CroatiaMap <- ggmap(get_googlemap(center=cro_center, scale=2, zoom=7), extent="normal")
# CroatiaMap


ui <- dashboardPage(
    dashboardHeader(title = "Analiza statistike RH"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Podaci na karti RH", tabName = "kartaRH", icon = icon("map")),
        menuItem("Pojedina županija", tabName = "zupanija", icon = icon("location-arrow")),
        menuItem("Podaci prikazani grafom", tabName = "grafRH", icon = icon("bar-chart")),
        menuItem("Unos vlastitih podataka", tabName = "upload", icon = icon("cloud-upload"))
      )
    ),
    dashboardBody(
      tabItems(
        # Main tab
        tabItem(tabName = "dashboard",
                h1("Moja aplikacija"),
                p("opis moje aplikacije")
        ),
        # First tab content
        tabItem(tabName = "kartaRH",
                fluidRow(
                  box(plotOutput("plot1", height = 250)),
                  
                  box(
                    title = "Controls",
                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "zupanija",
                h2("Županija brrrm")
        ),
        
        # Third tab content
        tabItem(tabName = "grafRH",
                h2("Grafićiiiii")
        ),
        # Fourth tab content
        tabItem(tabName = "upload",
                h2("Učitvanje vlastitog seta podataka za prikaz nad kartom RH")
        )
        
        )
      
      
    )
  )


server <- function(input, output) {}

shinyApp(ui = ui, server = server)

