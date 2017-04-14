if (!require('leaflet')) install.packages("leaflet")
if (!require('magrittr')) install.packages("magrittr") 
if (!require('rgdal')) install.packages("rgdal") 
if (!require('maptools')) install.packages("maptools")
if (!require('ggplot2')) install.packages("ggplot2")

?leaflet


mapStates = map("county", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(21, alpha = 0.5), stroke = FALSE)


?colorQuantile


setCPLConfigOption("SHAPE_ENCODING", "")
data.shape<- readOGR(dsn="./data/exported_boundaries_Croatia",
                     layer="Croatia_AL6", stringsAsFactors=FALSE, use_iconv=TRUE,
                     encoding="CP1252")
setCPLConfigOption("SHAPE_ENCODING", NULL)
data.shape$LOCALNAME <- gsub("c", "č", data.shape$LOCALNAME)
data.shape$LOCALNAME[data.shape$LOCALNAME == "Medimurska"] <- "Međimurska"
data.shape$LOCALNAME

writePolyShape(data.shape, "Croatia_AL6")
writeOGR(data.shape, dsn = "./data/map", layer = "Croatia_AL6", driver="ESRI Shapefile", overwrite_layer = T)

proba <- readOGR(dsn="./data/map",
                 layer="Croatia_AL6", stringsAsFactors=FALSE, encoding = "UTF-8")
proba$LOCALNAME

zaposleni <- read.csv("./data/processed data/zaposlenost ukupno zaposleni.csv", stringsAsFactors = F)

names(zaposleni)[1] <- "LOCALNAME"

#izbacivanje RH i neraspoređeno
zaposleni <- zaposleni[-1,] 
zaposleni[3,1] <- "Sisačko-moslavačka"
rownames(zaposleni) <- NULL
data.shape <- data.shape[order(data.shape$LOCALNAME),]
zaposleni <- zaposleni[order(zaposleni$LOCALNAME),]

data.shape$LOCALNAME
zaposleni$LOCALNAME

pal <- colorNumeric("viridis", NULL)

rh_zaposleni <- merge(data.shape, zaposleni, by="LOCALNAME")
rh_zaposleni$X2010.

## WORKS :3 --> gif in C:\Users\Ivyclover\Dropbox\DIPLOMSKI RH i zaposleni u 2010.
leaflet(data = rh_zaposleni) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(`X2010.`),
              label = ~paste0(LOCALNAME, ": ", formatC(`X2010.`, big.mark = ".", decimal.mark=",")),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addLegend(pal = pal, values = ~log10(`X2010.`), title="Broj zaposlenih u 2010.", opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

turizam_domaci <- read.csv("./data/processed data/turizam broj dolazaka domaći.csv", stringsAsFactors = F)
#izbacivanje RH 
turizam_domaci <- turizam_domaci[-1,]
names(turizam_domaci)[1] <- "LOCALNAME"
turizam_domaci <- turizam_domaci[order(turizam_domaci$LOCALNAME),]
rownames(turizam_domaci) <- NULL

rh_turizam_domaci <- merge(data.shape, turizam_domaci, by="LOCALNAME")
leaflet(data = rh_turizam_domaci) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(`X1994.`),
              label = ~paste0(LOCALNAME, ": ", formatC(`X1994.`, big.mark = ".", decimal.mark=",")),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addLegend(pal = pal, values = ~log10(`X1994.`), title="Broj domaćih turista u 1994.", opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))


nesrece_ukupno <- read.csv("./data/processed data/transport broj prometnih nesreća ukupno.csv", stringsAsFactors = F)
#izbacivanje RH 
nesrece_ukupno <- nesrece_ukupno[-1,]
names(nesrece_ukupno)[1] <- "LOCALNAME"
nesrece_ukupno <- nesrece_ukupno[order(nesrece_ukupno$LOCALNAME),]
rownames(nesrece_ukupno) <- NULL

rh_nesrece <- merge(data.shape, nesrece_ukupno, by="LOCALNAME")
rh_nesrece$X2004.

pal <- colorNumeric("RdYlBu", NULL)

leaflet(data = rh_nesrece) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(`X2004.`),
              label = ~paste0(LOCALNAME, ": ", formatC(`X2004.`, big.mark = ".", decimal.mark=",")),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addLegend(pal = pal, values = ~log10(`X2004.`), title="Ukupni broj prometnih nesreća u 2004.", opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))


rh_nesrece[names(rh_nesrece) == "X2004."]
names(rh_nesrece)
sele <- "X2004."

colnames(rh_nesrece@data)
rh_nesrece@data[colnames(rh_nesrece@data)  == "X2004."]
names(nesrece_ukupno)[-(1:2)]
rh_nesrece@data[,"X2004."]
