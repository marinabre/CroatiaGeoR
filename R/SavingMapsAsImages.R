if (!require('rgdal')) install.packages('rgdal')
library(rgdal)
if (!require('devtools')) install.packages('devtools')
devtools::install_github('rstudio/leaflet')
devtools::install_github('r-spatial/mapview', force = T)
require(devtools)
library(ggplot2)
library(mapview)
if (!require('tidyr')) install.packages("tidyr")
if (!require('magrittr')) install.packages("magrittr") 
require("plyr")

source_files <-list.files(path = "./shiny-app/data", pattern = "*.csv", all.files = T, full.names = T, recursive = F)
setCPLConfigOption("SHAPE_ENCODING", "")
counties_RH <- readOGR(dsn="./shiny-app/data", layer="Croatia_AL7", stringsAsFactors=F, use_iconv=T, encoding = "UTF-8")

 
podaci <- read.csv("./shiny-app/data/relativno/transport broj nastradalih u prometu s poginulim osobama.csv", stringsAsFactors = F, encoding = "UTF-8")
names(podaci)[1] <- "LOCALNAME"
podaci <- podaci[!(podaci$LOCALNAME == "Republika Hrvatska" | podaci$LOCALNAME == "Ukupno"),] 
spojeno <- merge(counties_RH, podaci, by="LOCALNAME")

minimum <- min(podaci[,-(1:2)])
maximum <- max(podaci[,-(1:2)])
pal <- colorNumeric("Reds", c(minimum,maximum))

map <- leaflet(data = spojeno, options = leafletOptions(minZoom = 7, maxZoom = 9)) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(spojeno@data[,'X2004.']),
              label = ~LOCALNAME,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addLegend("bottomright", pal = pal, values = ~spojeno@data[,'X2004.'],
            title = "broj poginulih u prometu 2004.",
            opacity = 1, labFormat = labelFormat( transform = function(x) 1000 * x, suffix="‰", digits=4)
  )
map
mapshot(map, file = paste0(getwd(), "/text/poginuli_rel.png"), remove_url = F, selfcontained = FALSE)

save_Maps <- function(source, dest, uzorak, paleta){
  for(srcFile in source){
    podaci <- read.csv(srcFile, stringsAsFactors = F, encoding = "UTF-8")
    names(podaci)[1] <- "LOCALNAME"
    podaci <- podaci[!(podaci$LOCALNAME == "Republika Hrvatska" | podaci$LOCALNAME == "Ukupno"),] #potrebno ukloniti radi bojevne skale
    rownames(podaci) <- NULL
    minimum <- min(podaci[,-(1:2)],na.rm=T)
    maximum <- max(podaci[,-(1:2)],na.rm=T)
    pal <- colorNumeric(paleta, c(minimum,maximum))
    spojeno <- merge(counties_RH, podaci, by="LOCALNAME")
    legend_lable <- gsub(".csv", "", gsub(paste0("./shiny-app/data",uzorak,"/[a-zA-Z]* "), "", srcFile)) #mijenjati ovisno koji je source
    
    years <- names(podaci)[-(1:2)]
    for(year in years){
      map <- leaflet(data = spojeno, options = leafletOptions(minZoom = 7, maxZoom = 9)) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(spojeno@data[,year]))%>%
        addLegend("bottomright", pal = pal, values = ~spojeno@data[,year],
                  title = paste(legend_lable, gsub( "X", "", year)),
                  opacity = 1
        )
      mapshot(map, file = paste0(getwd(), dest, legend_lable, year, ".png"), remove_url = T)
    }
  }
}

save_Maps(source_files, "/figuresAps/","", c("red", "grey", "green"))
source_files<-list.files(path = "./shiny-app/data/relativno", pattern = "*.csv", all.files = T, full.names = T, recursive = F)
save_Maps(source_files, "/figuresRel/", "/relativno", c("red", "grey", "green"))

source_files[grepl("grad", source_files)]

save_Maps(source_files[grepl("turizam", source_files)], "/figuresRel/", "/relativno", "Blues")
save_Maps(source_files[grepl("obrazovanje", source_files)], "/figuresRel/", "/relativno", "Blues")
save_Maps(source_files[grepl("kultura", source_files)], "/figuresRel/", "/relativno", "Blues")


save_Maps(source_files[grepl("zastita", source_files)], "/figuresRel/", "/relativno", "Greens")
save_Maps(source_files[grepl("izvoz", source_files)], "/figuresRel/", "/relativno", "Greens")


save_Maps(source_files[grepl("transport broj", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("uvoz", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj odseljenih", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj umrlih", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj razvedenih", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj odseljenih", source_files)], "/figuresRel/", "/relativno", "Reds")
save_Maps(source_files[grepl("zaposlenost nezaposleni", source_files)], "/figuresRel/", "/relativno", "Reds")

save_Maps(source_files[grepl("grad", source_files)], "/figuresRel/", "/relativno", "YlOrBr")
save_Maps(source_files[grepl("cesta", source_files)], "/figuresRel/", "/relativno", "YlOrBr")


source_files<-list.files(path = "./shiny-app/data/relativno", pattern = "*.csv", all.files = T, full.names = T, recursive = F)
save_Maps(source_files[grepl("industrija", source_files)], "/figuresRel/", "/relativno", "YlOrBr")

save_Maps(source_files[grepl("luka", source_files)], "/figuresRel/","/relativno", "Blues")
save_Maps(source_files[grepl("osig", source_files)], "/figuresRel/","/relativno", "Greens")


source_files <-list.files(path = "./shiny-app/data", pattern = "*.csv", all.files = T, full.names = T, recursive = F)

save_Maps(source_files[grepl("turizam", source_files)], "/figuresAps/","", "Blues")
save_Maps(source_files[grepl("obrazovanje", source_files)], "/figuresAps/","", "Blues")
save_Maps(source_files[grepl("kultura", source_files)], "/figuresAps/","", "Blues")
save_Maps(source_files[grepl("luka", source_files)], "/figuresAps/","", "Blues")


save_Maps(source_files[grepl("zastita", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("izvoz", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("doseljeni", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("sklopljeni", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("zivorodenih", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("ukupno zap", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("obrt", source_files)], "/figuresAps/","", "Greens")
save_Maps(source_files[grepl("pravn", source_files)], "/figuresAps/","", "Greens")

save_Maps(source_files[grepl("transport broj", source_files)], "/figuresAps/","", "Reds")
save_Maps(source_files[grepl("uvoz", source_files)], "/figuresAps/", "", "Reds")

save_Maps(source_files[grepl("stanovnistvo broj odseljenih", source_files)], "/figuresAps/", "", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj umrlih", source_files)], "/figuresAps/", "", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj razvedenih", source_files)], "/figuresAps/", "", "Reds")
save_Maps(source_files[grepl("stanovnistvo broj odseljenih", source_files)], "/figuresAps/", "", "Reds")
save_Maps(source_files[grepl("zaposlenost nezaposleni", source_files)], "/figuresAps/", "", "Reds")
save_Maps(source_files[grepl("zaposlenost stopa", source_files)], "/figuresAps/", "", "Reds")

save_Maps(source_files[grepl("grad", source_files)], "/figuresAps/", "", "YlOrBr")
save_Maps(source_files[grepl("cesta", source_files)], "/figuresAps/", "", "YlOrBr")
save_Maps(source_files[grepl("industrija", source_files)], "/figuresAps/", "", "YlOrBr")
