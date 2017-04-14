
header <- dashboardHeader(title = "Analiza statistike RH")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Podaci na karti RH", tabName = "kartaRH", icon = icon("map")),
    menuItem("Pojedina županija", tabName = "zupanija", icon = icon("location-arrow")),
    menuItem("Podaci prikazani grafom", tabName = "grafRH", icon = icon("bar-chart")),
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
    tabItem(tabName = "kartaRH",
            fluidRow(
              selectInput("izvor", "Izvor podataka", lista_izvora, selected = "./data/transport broj prometnih nesreca ukupno.csv"),
              div(class="outer",
                  tags$style(type = "text/css", ".outer {position: fixed; top: 150px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0, margin: 0 auto;}"),
                  leafletOutput("mymap", width = "100%", height = "95%")
                  ),
              absolutePanel(top = 50, right = 10,
                            selectInput("colors", "Color Scheme",
                                        rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "Reds"
                            ),
                            selectInput("godine", "Godine podataka",
                                        inicijalne_godine, selected = "X2014.")
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


ui <- dashboardPage(header, sidebar, body)