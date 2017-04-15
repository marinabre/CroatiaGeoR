  lista_izvora <-list.files(path = "./data", pattern = "*.csv", all.files = T,
             full.names = T, recursive = FALSE)
  
  zupanije_RH <- readOGR(dsn="./data",
                   layer="Croatia_AL6", stringsAsFactors=FALSE, encoding = "UTF-8")
  
  
  inicijalne_godine <- names(read.csv("./data/transport broj prometnih nesreca ukupno.csv", stringsAsFactors = F))[-(1:2)]
  legenda_textovi <- scan("./data/Legenda.txt", what="", sep="\n", encoding = "UTF-8")