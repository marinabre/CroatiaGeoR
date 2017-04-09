install.packages('tigris')
library(tigris)

install.packages('rgdal')
library(rgdal)
require("maptools")
require("ggplot2")
require("plyr")
library(tidyverse)


# zavod za ststistiku linkovi: http://www.dzs.hr/Hrv_Eng/Pokazatelji/Turizam.xlsx
setCPLConfigOption("SHAPE_ENCODING", "")
data.shape<- readOGR(dsn="./data/exported_boundaries_Croatia",
                     layer="Croatia_AL6", stringsAsFactors=FALSE, use_iconv=TRUE,
                     encoding="CP1252")
setCPLConfigOption("SHAPE_ENCODING", NULL)

#iconv(pt4$NAME_1, from="CP1252", to="UTF-8")

plot(data.shape) 
rownames(data.shape@data)
summary(data.shape)
glimpse(data.shape)
data.shape$LOCALNAME <- gsub("c", "č", data.shape$LOCALNAME)
data.shape$LOCALNAME[is.na(data.shape$LOCALNAME)] <- "Međimurska"
data.shape$LOCALNAME



setCPLConfigOption("SHAPE_ENCODING", "")
data.shape3<- readOGR(dsn="./data/exported_boundaries_Croatia",
                     layer="Croatia_AL6", stringsAsFactors=FALSE, use_iconv=TRUE,
                     encoding="CP1252")
setCPLConfigOption("SHAPE_ENCODING", NULL)
plot(data.shape3)
rownames(data.shape3@data)
data.shape3$LOCALNAME <- gsub("c", "č", data.shape3$LOCALNAME)
data.shape3$LOCALNAME[is.na(data.shape3$LOCALNAME)] <- "Međimurska"
data.shape3@data$id = rownames(data.shape3@data)
data.shape3.points = fortify(data.shape3, region="id")
data.shape3.df = join(data.shape3.points, data.shape3@data, by="id")
summary(data.shape3)
glimpse(data.shape3.df)


ggplot(data.shape3.df) +
  aes(long,lat,group=group,fill=LOCALNAME) +
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  theme_bw()

# natural<-readOGR(dsn="C:/Users/Ivyclover/Dropbox/DIPLOMSKI/croatia-latest.shp",layer="natural")
# plot(natural)
# rownames(natural@data)
# natural@data$id = rownames(natural@data)
# natural.points = fortify(natural, region="id")
# natural.df = join(natural.points, natural@data, by="id")
# summary(natural)
# glimpse(natural.df)
# unique(natural$TYPE_1)




stanovnistvo <- read.csv("./data/StanovnistvoPutovanjePoZupanijama.csv")
stanovnistvo$X2014.Privatno.Putovanja.Inozemstvo.1.i.više.noćenja

#uklanjanja riječi " županija" iz naziva županija
stanovnistvo$Zupanija <- unlist(lapply(stanovnistvo$Zupanija, function(x) {
  gsub(" županija", "", x)
}))


zupanije <- tbl_df(data.shape)
glimpse(zupanije)

?join
spojeno <- join(zupanije, stanovnistvo, by = zupanije$NAME_1)
ggplot(spojeno) + 
  aes(long,lat,group=group,fill=X2014.Privatno.Putovanja.Inozemstvo.1.i.više.noćenja) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  theme_bw()
