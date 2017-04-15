

server <- function(input, output, session) {
  #source("global.R")

  podaci_input <- reactive({
    podaci <- read.csv(input$izvor, stringsAsFactors = F)
    names(podaci)[1] <- "LOCALNAME"
    podaci <- podaci[order(podaci$LOCALNAME),]
    rownames(podaci) <- NULL
    podaci
  })
    
    
  dat_source <- reactive({
    merge(zupanije_RH, podaci_input(), by="LOCALNAME")
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, NULL)
  })
  
  odabrana_godina <- reactive({
    dat_source()@data[,input$godine]
  })
  
  godina_text <- reactive({
    gsub("X", "", input$godine)
  })
  
  legend_title <- reactive({
    text <- paste(gsub(".csv", "", gsub("./data/[a-zA-z]* ", "", input$izvor)), godina_text())
    #prvo slovo veliko
    paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
  })
    
  output$mymap <- renderLeaflet({
    leaflet(data = dat_source(), options = leafletOptions(minZoom = 5, maxZoom = 10)) %>%
      setView(zoom = 7, lng = 15, lat=43) %>%
      fitBounds(lat1=42, lng1=10, lat2=46, lng2=18) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~colorpal()(odabrana_godina()),
                  label = ~paste0(LOCALNAME, ": ", formatC(odabrana_godina(), big.mark = ".", decimal.mark=",")),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(position = "bottomright", pal = colorpal(), 
                values = ~log10(odabrana_godina()), title=legend_title(), 
                opacity = 1.0, labFormat = labelFormat(transform = function(x) round(10^x)))
  })
  
  observe({
    
    change <- input$izvor
    
    if(change != "./data/transport broj prometnih nesreca ukupno.csv"){
      
      updateSelectInput(session, "godine",
                        label = "Godine podataka",
                        choices = names(podaci_input())[-(1:2)], selected = "X2014."
      )
    }
    data <- dat_source()
    leafletProxy("mymap", data = data)  %>% clearControls() %>% clearShapes() %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~colorpal()(odabrana_godina()),
                  label = ~paste0(LOCALNAME, ": ", formatC(odabrana_godina(), big.mark = ".", decimal.mark=",")),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(position = "bottomright", pal = colorpal(), 
                values = ~log10(odabrana_godina()), title=legend_title(), 
                opacity = 1.0, labFormat = labelFormat(transform = function(x) round(10^x)))
    
  })
  
}