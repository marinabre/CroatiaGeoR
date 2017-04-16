

server <- function(input, output, session) {
  #source("global.R")

  data_input <- reactive({
    new_data <- read.csv(input$file_source, stringsAsFactors = F)
    names(new_data)[1] <- "LOCALNAME"
    #new_data <- new_data[order(new_data$LOCALNAME),]
    rownames(new_data) <- NULL
    new_data
  })
  
  plot_input <- reactive({
    temp <- gather(data_input(), year, shown_data, -LOCALNAME, -County.of)
    temp$year <- gsub("X", "", temp$year)
    temp
  })
  
  plot_input_county <- reactive({
    subset(plot_input(), LOCALNAME == input$counties)
  })
  
  plot_input_CRO_and_county <- reactive({
    subset(plot_input(), LOCALNAME == input$counties | LOCALNAME == "Republika Hrvatska")
  })
  
  pie_plot_input <- reactive ({
    subset(plot_input(), LOCALNAME != "Republika Hrvatska" & year == gsub("X", "", input$years2))
  })
  
  chosen_county <- reactive({
    match(input$counties, data_input()[,1])
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
  
  data_y_label <- reactive({
    gsub(".csv", "", gsub("./data/[a-zA-Z]* ", "", input$file_source))
  })
  
  pie_plot_legend <- reactive({
    text <- paste(data_y_label(), gsub("X", "", input$years2))
  })
  
  legend_title <- reactive({
    text <- paste(data_y_label(), year_text())
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
  
  output$barPlot <- renderPlotly({
      ggplot(plot_input_county(), aes(x = year, y = shown_data, fill = LOCALNAME)) + 
      geom_bar(stat = "identity")+
      labs(x = "Godina", y = data_y_label(), fill = "Županija")+
      theme(axis.text.x = element_text(angle = 90))
    })
  
  output$linePlot <- renderPlotly({
    ggplot(plot_input_CRO_and_county(), aes(x = year, y = shown_data, color=LOCALNAME, group = LOCALNAME))  + 
      geom_line()+ 
      geom_point()+
      labs(x = "Godina", y = data_y_label(), col = "Županija")+
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
  })
  
  output$piePlot <- renderPlotly({
    plot_ly(pie_plot_input(), labels = ~LOCALNAME, values = ~shown_data, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            #hoverinfo = 'text',
            #text = ~paste('$', shown_data, ' billions'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = pie_plot_legend(),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  #?theme
  observe({
    updated <- F
    if(input$file_source != source_files[1]){
      updateSelectInput(session, "years",
                        label = "Godine podataka",
                        choices = names(data_input())[-(1:2)], selected = tail(names(data_input())[-(1:2)], 1)
      )
      updateSelectInput(session, "years2",
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