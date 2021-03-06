if (!require('leaflet')) install.packages("leaflet")
if (!require('magrittr')) install.packages("magrittr") 
if (!require('rgdal')) install.packages("rgdal") 
if (!require('maptools')) install.packages("maptools")
if (!require('ggplot2')) install.packages("ggplot2")
require("plyr")
library(tidyverse)
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

#izbacivanje RH i nerasporedeno
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



##faceted data plots

podaci <- read.csv("./shiny-app/data/stanovnistvo procjena br st sredinom godine.csv", stringsAsFactors = F, encoding = "UTF-8")

names(podaci)[1] <- "LOCALNAME"
podaci <- podaci[order(podaci$LOCALNAME),]
rownames(podaci) <- NULL
names(podaci)[-(1:2)] <- as.numeric(gsub("X", "",names(podaci)[-(1:2)]))

mergano <- merge(counties_RH, podaci, by="LOCALNAME")

mergano@data$id = rownames(mergano@data)
mergano.points = fortify(mergano, region="id")
mergano.df = join(mergano.points, mergano@data, by="id")

head(mergano@data)
# names(podaci)[-(1:2)]
# mergano@data[,names(podaci)[-(1:2)][3]]
# grep("X2014.*", names(podaci)[-(1:2)], value = T)
date <- names(podaci)[-(1:2)]

ggplot(data = mergano.df, # the input data
       aes(x = long, y = lat, groups = groups)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
#  coord_equal() + # fixed x and y scales
  facet_wrap(~date ) + # one plot per time slice
  scale_fill_gradient2(low = "green", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure






london_data <- read.csv("C:/Users/Ivyclover/Downloads/census-historic-population-borough.csv")
library(tidyr) # if not install it, or skip the next two steps
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2) # check the output (not shown)

podaci <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = F, encoding = "UTF-8")[-1,-2]
ptidy <- gather(podaci, godina, pop, -Županija)
ptidy$godina <- gsub(".", "", gsub("X", "",ptidy$godina), fixed = T)
names(ptidy)[1] <- "LOCALNAME"
ptidy2 <- ptidy[order(ptidy$LOCALNAME),] 

karta <- counties_RH
karta@data$id = rownames(karta@data)
karta.points = fortify(karta, region="id")
karta.df = join(karta.points, karta@data, by="id")

unique(karta$LOCALNAME)
unique(ptidy$LOCALNAME)
mergano <- left_join(karta.df, ptidy2, by="LOCALNAME")

ggplot(data = mergano, # the input data
       aes(x = long, y = lat, fill = pop, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  facet_wrap(~godina) + # one plot per time slice
  scale_fill_gradient2(low = "red", mid = "grey", high = "green", # colors
                       midpoint = 0, name = "prirast") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure






prirast <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = F, encoding = "UTF-8")[,-2]
prirast_tdy <- gather(prirast, godina, pop, -Županija)
migracija <- read.csv("./shiny-app/data/stanovnistvo saldo migracije s inozemstvom.csv", stringsAsFactors = F) [,-2]
migracija_tdy <- gather(migracija, godina, pop, -Županija)

saldo <- merge(prirast_tdy, migracija_tdy, by = c("Županija", "godina"))
saldo <- transform(saldo, saldoPop = pop.x + pop.y)[,-(3:4)]
names(saldo)[1] <- "LOCALNAME"


mergano <- left_join(karta.df, saldo, by="LOCALNAME")

ggplot(data = mergano, # the input data
       aes(x = long, y = lat, fill = saldoPop, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  facet_wrap(~godina) + # one plot per time slice
  scale_fill_gradient2(low = "red", mid = "grey", high = "green", # colors
                       midpoint = 0, name = "Zbroj prirodnog prirasta i salda migracije") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure


prirast <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = F, encoding = "UTF-8")[,-2]
#prirast <- prirast[-1,]
prirast_tdy <- gather(subset(prirast, Županija =="Grad Zagreb" | Županija == "Republika Hrvatska"), godina, pop, -Županija)

ggplot(subset(prirast_tdy, Županija =="Grad Zagreb"), aes(x = godina, y = pop, fill = Županija)) + geom_bar(stat = "identity")





setCPLConfigOption("SHAPE_ENCODING", "") #neccesary so that the croatian symbols don't get squashed
counties_RH <- readOGR(dsn="./shiny-app/data",
                       layer="Croatia_AL7", stringsAsFactors=F, use_iconv=T, encoding = "UTF-8")

prirast <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = F, encoding = "UTF-8")[,-2]
names(prirast)[1] <- "LOCALNAME"
dat_source <- merge(counties_RH, prirast, by="LOCALNAME")


class(dat_source@data[,"X2015."])

class(prirast[,"X2015."])




prirast_tdy <- gather(prirast[-seq(1:15),], godina, pop, -Županija)

prirast_tdy$godina <- as.numeric(gsub("X", "", prirast_tdy$godina))

ggplot(prirast_tdy, aes(godina,pop , color = Županija, group= Županija)) + geom_point(na.rm = T, alpha = 0.6) +
  geom_line()+ 
  labs(x = "Godina", y = "prirodni prirast", "Županija" ) 


d <- density(prirast$X1998.)
plot(d, main="Gustoća prirasta u 1998.")

dotchart(prirast$X2014.,labels=prirast$Županija,cex=.7,
         main="Prirast po županijama", 
         xlab="Prirast stanovništva")

transponiraj_dataframe <- function(df){
  zupanije <- df$Županija
  
  # transponiraj sve osim prva dva stupca (imena zupanija)
  df <- as.data.frame(t(df[,-(1:2)]))
  colnames(df) <- zupanije
  rownames(df)<- gsub('X', '',rownames(df))
  
  df
}

prirast_2 <- transponiraj_dataframe(prirast)

podaci <- read.csv("./shiny-app/data/stanovnistvo prirodni prirast.csv", stringsAsFactors = T, encoding = "UTF-8")[,-2]
podaci <- podaci[-1,]
ptidy <- gather(podaci, godina, pop, -Županija)
ptidy$godina <- as.numeric(gsub(".", "", gsub("X", "",ptidy$godina), fixed = T))


ggplot(subset(ptidy, Županija == 'Splitsko-dalmatinska'), aes(x = godina, y = pop, fill = Županija)) + 
  geom_col()+
  labs(x = "Godina", y = "Prirast", fill = "Županija")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



ptidy[ptidy$Županija == 'Zagrebačka']





prirast[,1:2]
library(hexbin)
h <- hexbin(ptidy[,-1])
plot(h)
plot(h, colramp=colorRampPalette(rev(brewer.pal(11,'Spectral'))))


library(car) 
scatterplot(pop ~ godina, data=ptidy, 
            xlab="godina", ylab="prirast", 
            main="Napredni Scatter Plot", 
            labels=ptidy$Županija)


sm.density.compare(ptidy$pop,ptidy$godina, xlab="Miles Per Gallon") +
title(main="MPG Distribution by Car Cylinders")

