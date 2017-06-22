# if (!require('shiny')) install.packages("shiny")
# if (!require('shinydashboard')) install.packages("shinydashboard")
# if (!require('leaflet')) install.packages("leaflet")
# if (!require('rgdal')) install.packages("rgdal") 
# if (!require('ggplot2')) install.packages("ggplot2")
# if (!require('magrittr')) install.packages("magrittr") 
# if (!require('plotly')) install.packages("plotly")

library(shiny)
library(shinydashboard)
library(magrittr)
library(tidyr)
#if (!require('devtools')) install.packages('devtools')
library(crosstalk)
library(leaflet)
library(ggplot2)
library(plotly)

server <- function(input, output, session) {
  
  input_source_files <- reactive({
    if(input$relAps == TRUE)
      rel_source_files
    else
      source_files
  })

  output$show_warning <- output$show_warning1 <- output$show_warning2 <- renderUI({
    inFile <- input$file1
    tags$h3(class="warning",tags$style(type = "text/css", ".warning {color: red; position: relative; z-index:1000;}"), 
            ifelse(! is.null(inFile),paste("Prikazani podaci su iz učitane datoteke: \"", inFile$name, "\"!", sep=""), ""))
  })
  
  output$show_warning3 <- renderUI({
    if( length(which(plot_input()$shown_data < 0))>0){
      tags$h3(class="warning",tags$style(type = "text/css", ".warning {color: red; position: relative; z-index:1000;}"), 
              "Podaci iz odabrane datoteke imaju negativnih vrijednosti, tortni graf može prikazivati samo one pozitivne!")
    }
  })
  
  data_input <- reactive({
    inFile <- input$file1
    if(! is.null(inFile)){
      new_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote, stringsAsFactors = F, encoding = "UTF-8")
    }else{
      new_data <- read.csv(input$file_source, stringsAsFactors = F, encoding = "UTF-8")
    }
    names(new_data)[1] <- "LOCALNAME"
    rownames(new_data) <- NULL
    new_data
  })
  
  plot_input <- reactive({
    temp <- gather(data_input(), year, shown_data, -LOCALNAME, -County.of)
    temp$year <- gsub("X", "", temp$year)
    temp
  })
  
  plot_input_county_data <- reactive({
    subset(plot_input(), LOCALNAME == input$counties)
  })
  
  plot_input_county <- reactive({
    subset(counties_RH, LOCALNAME == input$counties)
  })
  
  plot_input_CRO_and_county <- reactive({
    subset(plot_input(), LOCALNAME == input$counties | LOCALNAME == "Republika Hrvatska")
  })
  
  pie_plot_input <- reactive ({
    subset(plot_input(), LOCALNAME != "Republika Hrvatska" & year == gsub("X", "", input$years2))
  })
  
  county_multiple_input <- reactive({
    subset(plot_input(), LOCALNAME %in% input$counties_multiple)
    
  })
  
  chosen_county <- reactive({
    match(input$counties, data_input()[,1])
  })
    
  dat_source <- reactive({
    merge(counties_RH, data_input(), by="LOCALNAME")
  })
  
  colorpal <- reactive({ #min & max are used here to ensure same color scale for all data in dataset
    help <- subset(data_input(), LOCALNAME != "Republika Hrvatska")
    colorNumeric(input$colors, c(min(help[,-(1:2)], na.rm = T), max(help[,-(1:2)], na.rm = T)))
  })
  
  chosen_year <- reactive({
    dat_source()@data[,input$years]
  })
  
  year_text <- reactive({
    gsub("X", "", input$years)
  })
  
  file_name <- reactive({
    inFile <- input$file1
    if(! is.null(inFile)){
      inFile$name
    }else{
      input$file_source
    }
  })
  
  data_y_label <- reactive({
    ifelse(input$relAps, paste(gsub(".csv", "", gsub("./data/relative/[a-zA-Z]* ", "", file_name())), " po stanovniku"), gsub(".csv", "", gsub("./data/[a-zA-Z]* ", "", file_name())) )
  })
  
  pie_plot_legend <- reactive({
    text <- paste(data_y_label(), gsub("X", "", input$years2))
  })
  
  legend_title <- reactive({
    text <- paste(data_y_label(), year_text())
    #prvo slovo veliko
    paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
  })
  
  output$countyPlot <- renderLeaflet({
      leaflet(data = plot_input_county()) %>%
      addTiles(options = tileOptions(minZoom = 7, maxZoom = 12)) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3, label = ~LOCALNAME)
    })

  output$mymap <- renderLeaflet({
    leaflet(data = dat_source(), options = leafletOptions(minZoom = 7, maxZoom = 9)) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, fillColor = ~colorpal()(chosen_year()),
                label = ~paste0(LOCALNAME, ": ", formatC(chosen_year(), big.mark = ".", decimal.mark=",")),
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
    addLegend(position = "bottomright", pal = colorpal(), 
              values = ~chosen_year(), title=legend_title(), 
              opacity = 1.0)
  })
  
  output$barPlot <- renderPlotly({
    plot_ly(plot_input_county_data(),
            x = plot_input_county_data()$year, 
            y = plot_input_county_data()$shown_data,
            type = "bar") %>% 
    layout(yaxis = list(title = data_y_label()), 
           xaxis = list(type="category", categoryorder="category ascending", tickangle=-35, title= paste("Podaci za", input$counties, "županiju po godinama")))
    })
  
  output$linePlot <- renderPlotly({
    ggplot(plot_input_CRO_and_county(), aes(x = year, y = shown_data, color=LOCALNAME, group = LOCALNAME))  + 
    geom_line() + 
    geom_point() +
    labs(x = "Godina", y = data_y_label(), col = "Županija") +
    theme_bw() +
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
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE) %>%
    layout(title = pie_plot_legend(),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$linePlot_multiple <- renderPlotly({
    ggplot(county_multiple_input(), aes(x = year, y = shown_data, color=LOCALNAME, group = LOCALNAME))  + 
    geom_line() + 
    geom_point() +
    labs(x = "Godina", y = data_y_label(), col = "Županija") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
  })
  
  file_name_gif <- reactive({
    if(input$relAps){
      paste0(gsub(".csv", "", gsub("./data/relative/", "", file_name())),".gif")
    }else{
      paste0(gsub(".csv", "", gsub("./data/", "", file_name())),".gif")
    }
  })
  
  output$gifJedan <- renderImage({
    fname <- paste("./www/gif/APS", file_name_gif())
    if (length(
        list.files(path = "./www/gif/", pattern = paste("APS", file_name_gif()), all.files = T, full.names = T, recursive = F)
        ) != 0) {
      list(src = fname, contentType = "image/gif")
    } else{
      list(src = "./www/nedostupan_gif.png", contentType = "image/png")
    }
    }, deleteFile=FALSE)
  
  
  output$gifDva <- renderImage({
    fname <- paste("./www/gif/REL", file_name_gif())
    if (length(
      list.files(path = "./www/gif/", pattern = paste("REL", file_name_gif()), all.files = T, full.names = T, recursive = F)
    ) != 0) {
      list(src = fname, contentType = "image/gif")
    } else{
      list(src = "./www/nedostupan_gif.png", contentType = "image/png")
    }
  }, deleteFile = FALSE)
  
  
  
  #?theme
  observe({
      updateSelectInput(session, "years",
                        choices = names(data_input())[-(1:2)], selected = tail(names(data_input())[-(1:2)], 1) )
      updateSelectInput(session, "years2",
                        choices = names(data_input())[-(1:2)], selected = tail(names(data_input())[-(1:2)], 1) )
      updateSelectInput(session, "counties",
                        label = "Odabir županije",
                        choices = data_input()[,1], selected = data_input()[,1][2] )
      updateSelectInput(session, "counties_multiple",
                        choices = data_input()[,1], selected = data_input()[,1][2] )
      
      #so it doesn't get hit every single time the user selects something 
      if((input$relAps && !grepl(".data/relative/", input$file_source)) || (input$relAps == F && grepl(".data/relative/", input$file_source)))
      {
        updateSelectInput(session, "file_source",
                          choices = input_source_files(), selected = input_source_files()[1] )
      }

   })
  
}