
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
          tags$style(type = "text/css", ".outer {position: fixed; top: 7%; left: 15%; right: 0; bottom: 0; overflow: hidden; padding: 0, margin: 0 auto;}"),
          leafletOutput("mymap", width = "100%", height = "95%")
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
          column(6,
            plotlyOutput("barPlot")
          ),
          column(6,
            plotlyOutput("linePlot"),
            verbatimTextOutput("event")
          )
        ),
        fluidRow(
          column(6,
            selectInput("years2", "Godine podataka",
                        initial_years, selected = tail(initial_years,1)),
            plotlyOutput("piePlot")
          )
        )
        #faceted graf kroz godine
        
        
      )
    ),
    
    # Third tab content
    tabItem(tabName = "graphCRO",
            h2("Grafićiiiii")
    ),
    # Fourth tab content
    tabItem(tabName = "upload",
            h2("Učitvanje vlastitog seta podataka za prikaz nad kartom RH")
    )
  )
)


ui <- dashboardPage(header, sidebar, body)