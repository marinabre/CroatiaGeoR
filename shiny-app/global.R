

source_files <-list.files(path = "./data", pattern = "*.csv", all.files = T,
             full.names = T, recursive = FALSE)
  
counties_RH <- readOGR(dsn="./data",
                 layer="Croatia_AL6", stringsAsFactors=FALSE, encoding = "UTF-8")
  
initial_data <- read.csv(source_files[1], stringsAsFactors = F)
initial_years <- names(initial_data)[-(1:2)]
counties_list <- initial_data[,1]

#  legenda_textovi <- scan("./data/Legenda.txt", what="", sep="\n", encoding = "UTF-8")