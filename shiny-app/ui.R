#if (!require('shiny')) install.packages("shiny")
#if (!require('shinydashboard')) install.packages("shinydashboard")
#if (!require('leaflet')) install.packages("leaflet")
#if (!require('ggplot2')) install.packages("ggplot2")
#if (!require('RColorBrewer')) install.packages("RColorBrewer") 
#if (!require('plotly')) install.packages("plotly")
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(plotly)
?encoding

#source("global.R")

header <- dashboardHeader(title = "Analiza statistike RH")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Podaci na karti RH", tabName = "mapCRO", icon = icon("map")),
    menuItem("Pojedina županija", tabName = "countyCRO", icon = icon("location-arrow")),
    menuItem("Podaci prikazani grafom", tabName = "graphCRO", icon = icon("bar-chart")),
    selectInput("file_source", "Izvor podataka", source_files, selected = source_files[1]),
    menuItem("Unos vlastitih podataka", tabName = "upload", icon = icon("cloud-upload"))
  )
)
?selectInput
body <- dashboardBody(
  tabItems(
    # Main tab
    tabItem(tabName = "dashboard",
            h1("Moja aplikacija"),
            p("opis moje aplikacije")
    ),
    # First tab content
    tabItem(tabName = "mapCRO",
      fluidPage(
        div(class="outer",
          tags$style(type = "text/css", ".outer {position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0, margin: 0 auto;}"),
          leafletOutput("mymap", width = "100%", height = "100%")
        ),
        absolutePanel(top = 50, right = 10,
                      selectInput("colors", "Color Scheme",
                                  rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "Reds"
                      ),
                      selectInput("years", "Godine podataka",
                                  initial_years, selected = tail(initial_years,1))
        )
      )
    ),
    
    # Second tab content
    tabItem(tabName = "countyCRO",
      fluidPage(
        fluidRow(
          h1("Podaci o županiji kroz godine"),
          selectInput("counties", "Odabir županije",
                      counties_list, selected = counties_list[2]
          )
        ),
        fluidRow(
          leafletOutput("countyPlot", width = "100%", height = "500px")
        ),
        fluidRow(
          column(6,
            plotlyOutput("barPlot")
          ),
          column(6,
            plotlyOutput("linePlot"),
            verbatimTextOutput("event")
          )
        )
      )
    ),

    # Third tab content
    tabItem(tabName = "graphCRO",
            h2("Podaci prikazani kroz različite vrste grafova"),
            fluidRow(
              column(5,
                selectInput("years2", "Godine podataka",
                           initial_years, selected = tail(initial_years,1)),
                plotlyOutput("piePlot")
              ),
              column(7,
                selectInput("counties_multiple", "Odabir županija za prikaz",
                                counties_list, selected = counties_list[2], multiple = T, width="100%"),
                p("Za brisanje odabrane vrijednosti, potrebno je kliknuti na nju i kliknuti 'Del' na tipkovnici"),
                plotlyOutput("linePlot_multiple")
            )
            #faceted graf kroz godine
    )
    ),
    # Fourth tab content
    tabItem(tabName = "upload",
      h1("Učitavanje vlastitog seta podataka za prikaz nad kartom RH"),
      h3("Struktura .csv datoteke kako bi aplikacija radila:"),
      tags$ul(
        tags$li("Imena županija su u 1. stupcu, engleski naziv moze biti u 2. stupcu ili se taj stupac ostavi praznim"),
        tags$li("Prvi unos je za Republiku Hrvatsku, ostale županije ne moraju pratiti nikakav redosljed"),
        tags$li("Novi unosi su stupci - aplikacija podrazumijeva da je ime stupca godina, no radit će ako i nije")
      ),
      p("Primjer .csv datoteke:"),
      tags$img(src = "primjer_csv_formata.png")
    )
  )
)


ui <- dashboardPage(header, sidebar, body)