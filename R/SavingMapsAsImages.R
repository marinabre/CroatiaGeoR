

source_files <-list.files(path = "./shiny-app/data", pattern = "*.csv", all.files = T,
                          full.names = T, recursive = F)

counties_RH <- readOGR(dsn="./shiny-app/data",
                       layer="Croatia_AL7", stringsAsFactors=F, use_iconv=T, encoding = "UTF-8")

podaci <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = F, encoding = "UTF-8")[-1,-2]
names(podaci)[1] <- "LOCALNAME"
spojeno <- merge(counties_RH, podaci, by="LOCALNAME")

glimpse(spojeno@data)
pal <- colorNumeric(c("red", "grey", "green"), NULL)



for(srcFile in source_files){
  podaci <- read.csv(srcFile, stringsAsFactors = F, encoding = "UTF-8")[-1,-2]
  names(podaci)[1] <- "LOCALNAME"
  spojeno <- merge(counties_RH, podaci, by="LOCALNAME")
  legend_lable <- gsub(".csv", "", gsub("./shiny-app/data/[a-zA-Z]* ", "", srcFile))
  
  years <- names(podaci)[-1]
  for(year in years){
    map <- leaflet(data = spojeno, options = leafletOptions(minZoom = 6, maxZoom = 9)) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(spojeno@data[,year]))%>%
      addLegend("bottomright", pal = pal, values = ~spojeno@data[,year],
                title = paste(legend_lable, gsub( "X", "", year)),
                opacity = 1
      )
    mapshot(map, file = paste0(getwd(), "/figures/", legend_lable, year, ".png"), remove_url = T)
  }
  
}

