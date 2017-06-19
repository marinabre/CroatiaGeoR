library(rgdal)

source_files <-list.files(path = "./data", pattern = "*.csv", all.files = T, full.names = T, recursive = F)
rel_source_files <- list.files(path = "./data/relative", pattern = "*.csv", all.files = T, full.names = T, recursive = F)

setCPLConfigOption("SHAPE_ENCODING", "") #neccesary so that the croatian symbols don't get squashed
counties_RH <- readOGR(dsn="./data", layer="Croatia_AL7", stringsAsFactors=F, use_iconv=T, encoding = "UTF-8")
  
initial_data <- read.csv(source_files[1], stringsAsFactors = F, encoding = "UTF-8")
initial_years <- names(initial_data)[-(1:2)]
counties_list <- initial_data[,1]

mycss <- "
  #plot-container {
    position: relative;
  }
  #loading-spinner {
    position: absolute;
    left: 50%;
    top: 50%;
    z-index: -1;
    margin-top: -33px;  /* half of the spinner's height */
    margin-left: -33px; /* half of the spinner's width */
  }
  #plot.recalculating {
    z-index: -2;
  }
  footer {
    clear: both;
    position: absolute;
    z-index: 10;
    bottom: 1%;
  }"