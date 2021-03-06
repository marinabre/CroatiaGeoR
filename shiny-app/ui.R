#if (!require('shiny')) install.packages("shiny")
#if (!require('shinydashboard')) install.packages("shinydashboard")
#if (!require('leaflet')) install.packages("leaflet")
#if (!require('ggplot2')) install.packages("ggplot2")
#if (!require('RColorBrewer')) install.packages("RColorBrewer") 
#if (!require('plotly')) install.packages("plotly")
#if (!require('devtools')) install.packages('devtools')
#devtools::install_github('rstudio/crosstalk')
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library(shiny)
library(shinydashboard)
#if (!require('gridExtra'))install.packages('gridExtra')
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
library(plotly)

header <- dashboardHeader(title = "Analiza statistike RH")
?dashboardHeader
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Podaci na karti RH", tabName = "mapCRO", icon = icon("map")),
    menuItem("Pojedina županija", tabName = "countyCRO", icon = icon("location-arrow")),
    menuItem("Podaci prikazani grafom", tabName = "graphCRO", icon = icon("bar-chart")),
    menuItem("Podaci prikazani gifom", tabName = "gifCRO", icon = icon("picture-o")),
    selectInput("file_source", "Skup podataka", source_files, selected = source_files[1]),
    menuItem("Unos vlastitih podataka", tabName = "upload", icon = icon("cloud-upload")),
    checkboxInput("relAps", "Relativni podaci", FALSE)
  )
)
?selectInput
body <- dashboardBody(
  tags$head(tags$style(HTML(mycss))),
  tabItems(
    # Main tab
    tabItem(tabName = "dashboard",
            h1("Analiza i vizualizacija geografskih podataka uz pomoć alata za statističko programiranje"),
            p("Danas je dostupno sve više relevantnih podataka vezanih uz različite geografske lokacije, odnosno područja, u Republici Hrvatskoj."),
            p("Primjeri tih podataka su bruto domaći proizvod ostvaren u određenim vremenskim razdobljima  u različitim županijama, zatim podaci vezani uz različite gospodarske aktivnosti, populaciju, obrazovanje, okoliš i drugo."),
            p("Kako bi se na osnovu dostupnih podataka mogle donositi što bolje odluke, odnosno, podatke što bolje razumjeti, važno je podatke prikazati na vizualno primjeren način."),
            p("S druge strane, danas postoje programski jezici koji omogućuju naprednu analizu i obradu podataka."),
            p("Zadatak ovog rada je  istražiti mogućnosti programskog jezika R za statističko programiranje te implementirati funkcionalnost koja će omogućiti prikaz podataka vezanih uz određena područja Republike Hrvatske."),
            p(" Za određene primjere dostupnih podataka potrebno je predložiti te prikazati adekvatan i prihvatljiv način vizualizacije."),
            h2("Izvori podataka korišteni u izradi aplikacije:"),
            tags$li(tags$a(href ="www.dzs.hr", "Hrvatski državni zavod za statistiku")),
            tags$li(tags$a("https://osm.wno-edv-service.de/boundaries/")),
            tags$li(tags$a(href ="https://www.openstreetmap.org", "OpenStreetmap")),
            h2("Neki od važnijih korištenih R paketa:"),
            tags$li(tags$a(href ="https://cran.r-project.org/web/packages/shiny/index.html", "shiny")),
            tags$li(tags$a(href ="https://cran.r-project.org/web/packages/leaflet/index.html", "leaflet")),
            tags$li(tags$a(href ="https://cran.r-project.org/web/packages/plotly/index.html", "plotly"))
            
    ),
    # First tab content
    tabItem(tabName = "mapCRO",
      fluidPage(
        uiOutput("show_warning"),
        div(class="outer",
          tags$style(type = "text/css", ".outer {position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0, margin: 0 auto; z-index:100;}"),
          tags$img(src = "spinner.gif", id = "loading-spinner", height= 66, width=66),
          leafletOutput("mymap", width = "100%", height = "100%")
        ),
        absolutePanel(top = 50, right = 10, class= "absolute",
                      tags$style(type = "text/css", ".absolute {z-index:1000;}"),
                      selectInput("colors", "Bojevna skala",
                                  rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "Reds"),
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
          uiOutput("show_warning1"),
          selectInput("counties", "Odabir županije",
                      counties_list, selected = counties_list[2])
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
            uiOutput("show_warning2"),
            uiOutput("show_warning3"),
            fluidRow(
              column(5,
                selectInput("years2", "Godine podataka", initial_years, selected = tail(initial_years,1)),
                plotlyOutput("piePlot")
              ),
              column(7,
                selectInput("counties_multiple", "Odabir županija za prikaz", counties_list, selected = counties_list[2], multiple = T, width="100%"),
                p("Za brisanje odabrane vrijednosti, potrebno je kliknuti na nju i kliknuti 'Del' na tipkovnici"),
                plotlyOutput("linePlot_multiple")
              )
            #faceted graf kroz godine
    )
    ),
    tabItem(tabName = "gifCRO",
            tags$head(tags$style(
              type="text/css",
              "#gifJedan img {max-width: 100%; width: 100%; height: auto}"
            )),
            tags$head(tags$style(
              type="text/css",
              "#gifDva img {max-width: 100%; width: 100%; height: auto}"
            )),
            fluidRow(
              column(6,
                     h3("Apsolutni podaci")
              ),
              column(6,
                     h3("Relativni podaci (Po stanovniku županije)")
              )
            ),
            fluidRow(
              column(6,
                imageOutput("gifJedan")
              ),
              column(6,
                imageOutput("gifDva")
              )
            ),
            tags$footer("Napomena: U relativnim podacima su samo pokrivene godine iz skupa [2001.,2015.] jer za njih postoji podatak o broju stanovnika")
          ),
    # Fourth tab content
    tabItem(tabName = "upload",
      fluidPage(
        fluidRow(
          column(8,
            h1("Učitavanje vlastitog seta podataka za prikaz nad kartom RH"),
            h3("Struktura .csv datoteke kako bi aplikacija radila:"),
            tags$ul(
              tags$li("Imena županija su u 1. stupcu, engleski naziv može biti u 2. stupcu ili se taj stupac ostavi praznim"),
              tags$li("Prvi unos je za Republiku Hrvatsku, ostale županije ne moraju pratiti nikakav redosljed"),
              tags$li("Novi unosi su stupci - aplikacija podrazumijeva da je ime stupca godina, no radit će ako i nije"),
              tags$li("Aplikacija pretpostavlja da je datoteka spremljena u UTF-8 enkodingu, inače neće raditi")
            ),
            p("Primjer .csv datoteke:"),
            tags$img(src = "primjer_csv_formata.png")
          ),
          column(4,
            tags$hr(),
            tags$hr(),
            tags$hr(),
            fileInput('file1', 'Odaberi CSV datoteku', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            tags$hr(),
            checkboxInput('header', 'Naslovi (eng. header)', TRUE),
            radioButtons('sep', 'Separator', c(Zarez=',', 'Točka zarez'=';', Tabulator='\t'), ','),
            radioButtons('quote', 'Navodnici', c(Nema='', 'Dvostruki navodnici'='"', 'Jednostruki navodnici'="'"), '"')
          )
        )
      )
    )
  ))


ui <- dashboardPage(header, sidebar, body)