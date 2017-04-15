

server <- function(input, output, session) {
  #source("global.R")

  data_input <- reactive({
    new_data <- read.csv(input$file_source, stringsAsFactors = F)
    names(new_data)[1] <- "LOCALNAME"
    #new_data <- new_data[order(new_data$LOCALNAME),]
    rownames(new_data) <- NULL
    new_data
  })
    
    
  dat_source <- reactive({
    merge(counties_RH, data_input(), by="LOCALNAME")
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, NULL)
  })
  
  chosen_year <- reactive({
    dat_source()@data[,input$years]
  })
  
  year_text <- reactive({
    gsub("X", "", input$years)
  })
  
  legend_title <- reactive({
    text <- paste(gsub(".csv", "", gsub("./data/[a-zA-Z]* ", "", input$file_source)), year_text())
    #prvo slovo veliko
    paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
  })
    
  output$mymap <- renderLeaflet({
    leaflet(data = dat_source(), options = leafletOptions(minZoom = 5, maxZoom = 10)) %>%
      #setView(zoom = 7, lng = 15, lat=43) %>%
      #fitBounds(lat1=42, lng1=10, lat2=46, lng2=18) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~colorpal()(chosen_year()),
                  label = ~paste0(LOCALNAME, ": ", formatC(chosen_year(), big.mark = ".", decimal.mark=",")),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(position = "bottomright", pal = colorpal(), 
                values = ~log10(chosen_year()), title=legend_title(), 
                opacity = 1.0, labFormat = labelFormat(transform = function(x) round(10^x)))
  })
  
  observe({
    updated <- F
    if(input$file_source != source_files[1]){
      updateSelectInput(session, "years",
                        label = "Godine podataka",
                        choices = names(data_input())[-(1:2)], selected = tail(names(data_input())[-(1:2)], 1)
      )
      updateSelectInput(session, "counties",
                        label = "Odabir županije",
                        choices = data_input()[,1], selected = data_input()[,1][1]
      )
      updated <- T
    }
    if(!updated){
      new_data <- dat_source()
      leafletProxy("mymap", data = new_data)  %>% clearControls() %>% clearShapes() %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5, fillColor = ~colorpal()(chosen_year()),
                    label = ~paste0(LOCALNAME, ": ", formatC(chosen_year(), big.mark = ".", decimal.mark=",")),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLegend(position = "bottomright", pal = colorpal(), 
                  values = ~log10(chosen_year()), title=legend_title(), 
                  opacity = 1.0, labFormat = labelFormat(transform = function(x) round(10^x)))
    }
    })
  
}