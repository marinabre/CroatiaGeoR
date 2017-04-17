
install.packages('rgdal')
library(rgdal)
require("maptools")
require("ggplot2")
require("plyr")
library(tidyverse)

setwd(getSrcDirectory()[1])
# zavod za ststistiku linkovi: http://www.dzs.hr/Hrv_Eng/Pokazatelji/Turizam.xlsx
iconv(NULL, "CP1250", "UTF-8")
setCPLConfigOption("SHAPE_ENCODING", "")
data.shape<- readOGR(dsn="./shiny-app/data",
                     layer="Croatia_AL7", stringsAsFactors=F, use_iconv=T,
                     encoding="UTF-8")

data.shape$LOCALNAME

?iconv
iconv(NULL, "CP1252", "UTF-8")
drops <- c("ADMIN_LVL", "CITY_KEY", "REGION_KEY", "NOTE","FLAG","CURRENCY",
           "ISO1","ISO2","WIKIPEDIA", "OFF_NAME", "TAGS")
data.shape@data <- data.shape@data[,!(names(data.shape@data) %in% drops)]
data.shape$LOCALNAME <- gsub("č", "c", data.shape$LOCALNAME)
data.shape$LOCALNAME[data.shape$LOCALNAME == "Međimurska"] <- "Medimurska"


names(data.shape@data)

data.shape$LOCALNAME <- gsub("c", "č", data.shape$LOCALNAME)
data.shape$LOCALNAME[data.shape$LOCALNAME == "Medimurska"] <- "Međimurska"
data.shape$LOCALNAME
data.shape <-  data.shape[order(data.shape$LOCALNAME),]
writePolyShape(data.shape, "Croatia_AL7")
writeOGR(data.shape, dsn = "./data/map", layer = "Croatia_AL7", driver="ESRI Shapefile", overwrite_layer = T, encoding = "UTF-8")
?writeOGR
####



data.shape<- readOGR(dsn="./shiny-app/data",
                     layer="Croatia_AL6", stringsAsFactors=F, use_iconv=T,
                     encoding="CP1250")

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


data.shape3$LOCALNAME

izvoz <- read.csv("./data/processed data/izvoz u tisucama eura.csv")
izvoz$Županija

# first remember the names
n <- izvoz$Županija

# transpose all but the first column (name)
izvoz <- as.data.frame(t(izvoz[,-(1:3)]))
colnames(izvoz) <- n



zupanije <- tbl_df(data.shape3)
glimpse(izvoz)

?join
spojeno <- join(zupanije, izvoz, by = zupanije$LOCALNAME)
ggplot(spojeno) + 
  aes(long,lat,group=group,fill=X2014.Privatno.Putovanja.Inozemstvo.1.i.više.noćenja) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  theme_bw()
