install.packages('xlsx')
library(xlsx)
require(xlsx)
library(dplyr)

#### trimws(x) ---> uklanja leading & trailing spaces
ocisti_dataframe <- function(df){
  df[df == "-"] <- NA
  br_stupaca <- length(names(df))
  df[,3:br_stupaca] <- sapply(df[,3:br_stupaca], function(x){as.numeric(gsub(' ', '',x))})
  df[,1] <- trimws(gsub(" županija","",df[,1]))
  df[,2] <- trimws(df[,2])
  names(df) <- gsub('X', '',names(df))
  rownames(df) <- NULL
  df
}

#transponiranje podataka kako bi se novi unos unosio kao redak a ne kao stupac :)
transponiraj_dataframe <- function(df){
  zupanije <- df$zupanija
  
  # transponiraj sve osim prva dva stupca (imena zupanija)
  df <- as.data.frame(t(df[,-(1:2)]))
  colnames(df) <- zupanije
  rownames(df)<- gsub('X', '',rownames(df))
  
  df
}



# Robna razmjena s inozemstvom.xlsx ############################# 

izvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                   startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,19))

# ako zelimo ukloniti samo  prva dva prazna retka, izvoz[-seq(1,3), ] ako zelimo ukloniti i podatke za cijelu drzavu
izvoz <- izvoz[-c(1,2), ]
names(izvoz)[1] <- "Županija"
names(izvoz)[2] <- "County.of"
#u xlsx fileu 2016 ima footnote koji se u Ru ispisuje kao "X2016.1............."
names(izvoz)[19] <- "X2016."
izvoz <- ocisti_dataframe(izvoz)

#stupci 2015. i 2016. su pomnoženi sa 1000 - greška u dzsu!!!
izvoz$`2015.` <- izvoz$`2015.` / 1000
izvoz$`2016.` <- izvoz$`2016.` / 1000


uvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                  startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,19))
names(uvoz) <- names(izvoz)
uvoz <- ocisti_dataframe(uvoz)

write.csv((izvoz), "./shiny-app/data/roba izvoz u tisucama kuna.csv", row.names=FALSE, fileEncoding = "UTF-8")

uvoz$`2015.` <- uvoz$`2015.` / 1000
uvoz$`2016.` <- uvoz$`2016.` / 1000

write.csv((uvoz), "./shiny-app/data/roba uvoz u tisucama kuna.csv", row.names=FALSE, fileEncoding = "UTF-8")



izvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                       startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,9))

izvozEuri <- izvozEuri[-c(1,2), ]
names(izvozEuri)[1:2] <- names(izvoz)[1:2]
names(izvozEuri)[9] <- "X2016."
izvozEuri <- ocisti_dataframe(izvozEuri)
## 2012. - 2016. su pomnožene sa 1000
izvozEuri[,5:9] <- izvozEuri[,5:9] / 1000

izvozEuri$Županija <- izvoz$Županija

uvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                      startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,9))

names(uvozEuri) <- names(izvozEuri)
uvozEuri <- ocisti_dataframe(uvozEuri)
uvozEuri$Županija <- izvoz$Županija
write.csv((izvozEuri), "./shiny-app/data/roba izvoz u tisucama eura.csv", row.names=FALSE, fileEncoding = "UTF-8")

## 2012. - 2016. su pomnožene sa 1000
uvozEuri[,5:9] <- uvozEuri[,5:9] / 1000
write.csv((uvozEuri), "./shiny-app/data/roba uvoz u tisucama eura.csv", row.names=FALSE, fileEncoding = "UTF-8")


# Bruto domaci proizvod.xlsx ############################# 
for(j in c(1,2)){
  for(i in c(1,2)){
    bdp <- read.xlsx("./data/statistika u nizu/Bruto domaci proizvod.xlsx", sheetName =ifelse(j==1,"12.1.2.1.", "12.1.2.2."),
                     startRow=7, encoding = "UTF-8", endRow=33, colIndex = seq(ifelse(i==1,1,19),ifelse(i==1,17,33)), stringsAsFactors=F)
    #retci 3 i 19 imaju podatke za kontinentalnu /Jadransku hrvatsku, 
    #to cemo ukloniti kako bi sacuvali uniformnost podataka, retci 2 i 18 su prazni
    bdp <- bdp[-c(2,3,18,19), ]
    if(i==1){
      names(bdp)[1:2] <- names(izvoz)[1:2]
      bdp[,1][1] <- "Republika Hrvatska"
      bdp_zup <- bdp[,1:2]
      bdp_names <- names(bdp)
    }else{
      bdp <- cbind(bdp_zup, bdp)
    }
    if(j==2) {
      bdp[,1:2] <- bdp_zup
      names(bdp) <- bdp_names
      }
    bdp <- ocisti_dataframe(bdp)
    write.csv(bdp, paste("./shiny-app/data/bdp BDP",ifelse(j==1,""," po stanovniku")," u tisucama ", ifelse(i==1, "kuna", "eura"), ".csv", sep = ""), row.names=FALSE, fileEncoding = "UTF-8")
  }
}

# Gradevinarstvo.xlsx ############################# 

ukupno_izdanih_dozvola <- read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.1.", 
                                    startRow=7, encoding = "UTF-8", endRow=30, 
                                    colIndex = seq(1,17), stringsAsFactors=F)
#uredni podaci :O
#visak je "zupanija" nakon naziva svake zupanije
#zadnji red umjesto "Bez lokacije zupanije2)"	Unclassified2) popraviti na "Nerasporedeno" radi konzistentnosti

ukupno_izdanih_dozvola[23,1] <- "Neraspoređeno"
ukupno_izdanih_dozvola[23,2] <- "Unclassified"
ukupno_izdanih_dozvola <- ocisti_dataframe(ukupno_izdanih_dozvola)
write.csv((ukupno_izdanih_dozvola), "./shiny-app/data/grad ukupno izdanih grad dozvola.csv", row.names=FALSE, fileEncoding = "UTF-8")

gradevina_stupci <- c(1, 19, 35, 50, 65, 80, 95)
gradevina_prvi_sheet <- c("za zgrade.csv", "za ostale gradevine.csv")
grad_zupanije_Unclassified <- ukupno_izdanih_dozvola[,1:2]

for(i in c(1,2)){
  sobni_stanovi <- ocisti_dataframe(
            read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.1.", 
                             startRow=7, encoding = "UTF-8", endRow=30, 
                             colIndex = seq(gradevina_stupci[i+1], gradevina_stupci[i+1]+ 14), 
                             stringsAsFactors=F))
  
  sobni_stanovi <- (cbind(grad_zupanije_Unclassified, sobni_stanovi))
  write.csv(sobni_stanovi, paste("./shiny-app/data/grad izdane grad dozvole", gradevina_prvi_sheet[i]), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(sobni_stanovi)

##3.2.2. - GRAdEVINSKE VELIcINE ZGRADA ZA KOJE SU IZDANE GRAdEVINSKE DOZVOLE (novogradnja i dogradnja)
gradevina_drugi_sheet <- c("ukupna velicina", "velicina stambenih", "velicina nestambenih")

for(i in c(1,2,3)){
  sobni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.2.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], ifelse(i==1, gradevina_stupci[i]+ 16, gradevina_stupci[i]+ 14)),
                             stringsAsFactors=F))
  if(i != 1){
    sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  }else{
    gradevina_zupanije <- sobni_stanovi[,1:2]
  }
  write.csv((sobni_stanovi), paste("./shiny-app/data/grad", gradevina_drugi_sheet[i], "zgrada s izdanom grad dozvolom.csv"), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(sobni_stanovi)

##3.2.3. - BROJ I POVRsINA STANOVA ZA KOJE SU IZDANE GRAdEVINSKE DOZVOLE
gradevina_treci_sheet <- c("broj stanova", "korisna povrsina m2 stanova")
for(i in c(1,2)){
  sobni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.3.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], ifelse(i == 1, gradevina_stupci[i]+ 16, gradevina_stupci[i]+ 14)), 
                             stringsAsFactors=F))
  
  if(i !=1) sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  
  write.csv((sobni_stanovi), paste("./shiny-app/data/grad", gradevina_treci_sheet[i], "s izdanom grad dozvolom.csv"), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(sobni_stanovi)

##3.2.4. - BROJ I GRAdEVINSKE VELIcINE ZAVRsENIH ZGRADA

ukupno_velicina_zavrsenih_zgrada <- read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.4.", 
                                              startRow=7, encoding = "UTF-8", endRow=30, 
                                              colIndex = seq(1,30), stringsAsFactors=F)
names(ukupno_velicina_zavrsenih_zgrada)
##header je rascjepkan kroz dva retka
##		                2002.		2003.		2004.		2005.		2006.		2007.		2008.		2009.		2010.		2011.		2012.		2013.		2014.		2015.	
#zupanija	County of	"broj Number"	"povrsina, m2 Floor area, m2"	...
names(ukupno_velicina_zavrsenih_zgrada)[1] <- ukupno_velicina_zavrsenih_zgrada[1,1]
names(ukupno_velicina_zavrsenih_zgrada)[2] <- ukupno_velicina_zavrsenih_zgrada[1,2]
#namjestanje headera da lici na nesto
names(ukupno_velicina_zavrsenih_zgrada)[c(FALSE, TRUE)][-1] <- paste(substr(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1],1,6), "povrsina, m2")
names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1] <- paste(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1], "broj")

ukupno_velicina_zavrsenih_zgrada <- ukupno_velicina_zavrsenih_zgrada[-c(1), ]
ukupno_velicina_zavrsenih_zgrada <- ocisti_dataframe(ukupno_velicina_zavrsenih_zgrada)

write.csv((ukupno_velicina_zavrsenih_zgrada), "./shiny-app/data/grad ukupna velicina zavrsenih zgrada s izdanom grad dozvolom.csv", row.names=FALSE, fileEncoding = "UTF-8")

for(i in c(1,2)){
  vel_zgrada <- read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.4.", 
                          startRow=9, encoding = "UTF-8", endRow=30, 
                          colIndex = seq(ifelse(i==1, 32, 61),ifelse(i==1, 59, 88)), 
                          stringsAsFactors=F, header = F)
  
  vel_zgrada <- cbind(ukupno_velicina_zavrsenih_zgrada[,1:2], vel_zgrada)
  names(vel_zgrada) <- names(ukupno_velicina_zavrsenih_zgrada)
  vel_zgrada <- (ocisti_dataframe(vel_zgrada))
  write.csv(vel_zgrada, paste("./shiny-app/data/grad", gradevina_drugi_sheet[i+1], "(zavrsenih) zgrada s izdanom grad dozvolom.csv"), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(vel_zgrada)
## 3.2.5. - ZAVRsENI STANOVI 
zavrseni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName ="3.2.5.", 
                              startRow=8, encoding = "UTF-8", endRow=30, 
                              colIndex = seq(1,30), stringsAsFactors=F))

names(zavrseni_stanovi) <- names(ukupno_velicina_zavrsenih_zgrada)
write.csv((zavrseni_stanovi), paste("./shiny-app/data/grad ukupni broj zavrsenih stanova s izdanom grad dozvolom.csv"), row.names=FALSE, fileEncoding = "UTF-8")


## 3.2.6. - ZAVRsENI STANOVI PREMA BROJU SOBA

grad_fja_obrada <- function(brojIteracija, sheet, pocetniRed, krajnjiRed, zupanija, sheetImena){
  for(i in brojIteracija){
    sobni_stanovi <- read.xlsx("./data/statistika u nizu/Gradevinarstvo.xlsx", sheetName =sheet, 
                               startRow=pocetniRed, encoding = "UTF-8", endRow=krajnjiRed, 
                               colIndex = seq(gradevina_stupci[i], ifelse(i==1, gradevina_stupci[i]+ 15, gradevina_stupci[i]+ 13)), stringsAsFactors=F)
    if(i != 1){
      sobni_stanovi <- cbind(zupanija, sobni_stanovi)
    }else{
      sobni_stanovi[,1] <- zupanija[,1]
    }
    sobni_stanovi <- (ocisti_dataframe(sobni_stanovi))
    write.csv(sobni_stanovi, paste("./shiny-app/data/grad", sheetImena[i], "s izdanom grad dozvolom.csv"), row.names=FALSE, fileEncoding = "UTF-8")
  }
}
gradevina_sesti_sheet <- c("1-sobni zavrseni stanovi", "2-sobni zavrseni stanovi", "3-sobni zavrseni stanovi", "4-sobni zavrseni stanovi", "5-sobni i visesobni zavrseni stanovi")
grad_fja_obrada(seq(1,5), "3.2.6.", 7, 29, gradevina_zupanije, gradevina_sesti_sheet)


## 3.2.7. - VRIJEDNOST IZVRsENIH GRAdEVINSKIH RADOVA PREMA VRSTI GRAdEVINA
#iste zupanije kao u 1. (ima unclassified na dnu)
#godine 2002-2015, okomito odvojenih setova podataka: 7
#Ukupno, Stambene zgrade, Nestambene zgrade, Prometna infrakstruktura, Cjevovodi, komunikacijski i elektricni vodovi, Slozene gradevine na industrijskim prostorima, Ostale nespomenute gradevine
gradevina_sedmi_sheet <- c("ukupna vrijednost", "vrijednost stambenih zgrada", "vrijednost nestambenih zgrada", "vrijednost prometne infrakstrukture", "vrijednost cjevovoda, kom i el vodova", "vrijednost slozenih gradevina", "vrijednost ostalih gradevina")
grad_fja_obrada(seq(1,7), "3.2.7.", 7, 30, grad_zupanije_Unclassified, gradevina_sedmi_sheet)

# Industrija.xlsx ############################# 
industrija <- (ocisti_dataframe(read.xlsx("./data/statistika u nizu/Industrija.xlsx", sheetName ="2.1.2.1.", 
                                                                startRow=9, encoding = "UTF-8", endRow=32, 
                                                                colIndex = seq(1,15), 
                                                                stringsAsFactors=F)))
industrija[4,1] <- "Sisačko-moslavačka"
write.csv(industrija, "./shiny-app/data/industrija ukupna vrijednost prodanih proizvoda po NP-u.csv", row.names=FALSE, fileEncoding = "UTF-8")


# Kultura.xlsx ############################# 
kultura_sheetovi <- c("8.3.1.","8.3.2.","8.3.3.","8.3.4.","8.3.5.")
kultura_podaci <- c("kina", "sjedala u kinima", "posjetitelja u kinima", "radiopretplatnika", "tv pretplatnika")

for(i in seq(1,5)){
  kultura <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Kultura.xlsx", sheetName =kultura_sheetovi[i], 
                          startRow=7, encoding = "UTF-8", endRow=29, 
                          colIndex = seq(1,25), 
                          stringsAsFactors=F))
  if(i == 1)
    kultura_names <- names(kultura)
  names(kultura) <- kultura_names
  write.csv((kultura), paste("./shiny-app/data/kultura broj ", kultura_podaci[i], ".csv", sep = ""), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(kultura)
rm(kultura_names)
# Obrazovanje.xlsx ############################# 
#prva tri sheeta - broj ustanova i broj djece okomito odvojeni, zupanije normalno raspisane
#zadnja dva ukupno i redovni 1.4. -akademske godine, 1.5. - obicne godine
obrazovanje_sheetovi_1 <- c("8.1.1.", "8.1.2.", "8.1.3.")
obrazovanje_podaci_1 <- c("djecji vrtici", "osnovne skole", "srednje skole")

for(i in c(1,2,3)){
  obrazovanje_ustanove <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                           startRow=7, encoding = "UTF-8", endRow=29, 
                           colIndex = seq(1, ifelse(i== 1, 13, 14)), 
                           stringsAsFactors=F))
  
  obrazovanje_djeca <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                            startRow=7, encoding = "UTF-8", endRow=29, 
                            colIndex = seq(ifelse(i==1,15,16),ifelse(i==1,25,27)), 
                            stringsAsFactors=F)
  
  obrazovanje_djeca <- cbind(obrazovanje_ustanove[,1:2], obrazovanje_djeca)
  obrazovanje_djeca <- ocisti_dataframe(obrazovanje_djeca)
  names(obrazovanje_djeca)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_djeca)[-(1:2)])
  names(obrazovanje_ustanove)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_ustanove)[-(1:2)])
  
  write.csv((obrazovanje_ustanove), paste("./shiny-app/data/obrazovanje", obrazovanje_podaci_1[i],"broj ustanova.csv"), row.names=FALSE, fileEncoding = "UTF-8")
  write.csv((obrazovanje_djeca), paste("./shiny-app/data/obrazovanje", obrazovanje_podaci_1[i], "broj djece.csv"), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(obrazovanje_ustanove)
rm(obrazovanje_djeca)
obrazovanje_sheetovi_2 <- c("8.1.4.", "8.1.5.")
obrazovanje_podaci_2 <- c("upisanih studenata.csv", "studenata koji su zavrsili.csv")
for(i in c(1,2)){
  obrazovanje_ukupno <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(1,ifelse(i==1,22,23)), 
                                  stringsAsFactors=F))
  
  obrazovanje_redovni <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(ifelse(i==1, 24, 25),ifelse(i==1,43,45)), 
                                  stringsAsFactors=F)
  
  obrazovanje_redovni <- cbind(obrazovanje_ukupno[,1:2], obrazovanje_redovni)
  obrazovanje_redovni <- ocisti_dataframe(obrazovanje_redovni)
  if(i == 1)
  {#micanje akademskih godina, pise samo prva godina
    names(obrazovanje_ukupno)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_ukupno)[-(1:2)])
    names(obrazovanje_redovni)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_redovni)[-(1:2)])
  }
  
  write.csv((obrazovanje_ukupno), paste("./shiny-app/data/obrazovanje ukupan broj", obrazovanje_podaci_2[i]), row.names=FALSE, fileEncoding = "UTF-8")
  write.csv((obrazovanje_redovni), paste("./shiny-app/data/obrazovanje broj redovnih", obrazovanje_podaci_2[i]), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(obrazovanje_ukupno)
rm(obrazovanje_redovni)


# Okolis.xlsx ############################# 
#3 seta okomito odvojenih podataka
okolis_stupci <- c(1,12,21)
okolis_imena <- c("tekuci izdaci.csv", "prihodi.csv", "investicije.csv")

for(i in c(1,2,3)){
  okolis <- read.xlsx("./data/statistika u nizu/Okolis.xlsx", sheetName ="6.1.1.", 
                      startRow=9, encoding = "UTF-8", endRow=32, 
                      colIndex = seq(okolis_stupci[i], ifelse(i==1, okolis_stupci[i]+ 9, okolis_stupci[i]+ 7)), 
                      stringsAsFactors=F)
  okolis <- okolis[-c(2),]
  if(i != 1){
    okolis <- cbind(okolis_zupanije, okolis)
  }else{
    okolis[4,1] <- "Sisačko-moslavačka" #ispravljanje tipfelera
    okolis_zupanije <- okolis[,1:2]
  }
  names(okolis)[10] <- "X2015." #micanje anotacije
  okolis <- (ocisti_dataframe(okolis))
  write.csv(okolis, paste("./shiny-app/data/zastita okolisa", okolis_imena[i]), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(okolis)

# Stanovnistvo.xlsx #############################
#jedini odmah iskoristivi skup podataka je na 7.2.3. sheetu, ovo ostalo ce potrajati malo

#RH 8-13, Zagrebacka  15-20, Krapinska 22-27... Zg 155-160
st_broj_zupanija_prvi <- seq(8, 155, 7)

#trebaju nam headeri
stanovnistvo_names <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovnistvo.xlsx", sheetName = "7.4.1.",
          startRow=7, encoding = "UTF-8", endRow=8,
          colIndex = seq(1,20),
          stringsAsFactors=F))

#lista, svaki clan liste je ujedno dataframe sa podacima za pojedinu zupaniju
stanovnistvo_prirodno_kretanje <- list()
stanovnistvo_prirodno_kretanje <- lapply(st_broj_zupanija_prvi, function(i){
  read.xlsx("./data/statistika u nizu/Stanovnistvo.xlsx", sheetName = "7.4.1.", 
            startRow=i, encoding = "UTF-8", endRow=i+5, 
            colIndex = seq(1,20), 
            stringsAsFactors=F)
})

head(stanovnistvo_prirodno_kretanje)

#za svaki set podataka definiramo zasebni dataframe, stavljamo prazan al sa imenima koja nam trebaju
st_zivorodeni <- stanovnistvo_names[F,]
st_umrli <- stanovnistvo_names[F,]
st_prirast <- stanovnistvo_names[F,]
st_sklopljeni_brakovi <- stanovnistvo_names[F,]
st_razvedeni_brakovi <- stanovnistvo_names[F,]


for(i in seq(1,22)){
  pom <- ocisti_dataframe(data.frame(stanovnistvo_prirodno_kretanje[i]))

  #kako bi pokupili naziv zupanije u svaki redak
  for(j in seq(1,5))    pom[j,][1:2] <- trimws(gsub(".", " ",names(pom)[1:2], fixed = TRUE))
  
  #nedostaju - u imenima zupanija (jer su bili u names pa im je R zamijenio s .), no grad zagreb i RH ne trebaju -
  if(pom[,1][1] != "Grad Zagreb" && pom[,1][1] != "Republika Hrvatska")
  {
    pom[,1] <- gsub(" ", "-",pom[,1])
  }
  #imena moraju biti konzistentna kako bi se moglo dodati na kraj dataframea
  names(pom) <- names(stanovnistvo_names)
  st_zivorodeni <- bind_rows(st_zivorodeni, pom[1,])
  st_umrli <- bind_rows(st_umrli, pom[2,])
  st_prirast <- bind_rows(st_prirast, pom[3,])
  st_sklopljeni_brakovi <- bind_rows(st_sklopljeni_brakovi, pom[4,])
  st_razvedeni_brakovi <- bind_rows(st_razvedeni_brakovi, pom[5,])
}
rm(pom)

write.csv((st_zivorodeni), "./shiny-app/data/stanovnistvo broj zivorodenih.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_umrli), "./shiny-app/data/stanovnistvo broj umrlih.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_prirast), "./shiny-app/data/stanovnistvo prirodni prirast.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_sklopljeni_brakovi), "./shiny-app/data/stanovnistvo broj sklopljenih brakova.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_razvedeni_brakovi), "./shiny-app/data/stanovnistvo broj razvedenih brakova.csv", row.names=FALSE, fileEncoding = "UTF-8")

##7.2.2. DOSELJENO I ODSELJENO STANOVNIsTVO
#taman kada pomislis da je gornji sheet nesto najgore ikad...
stanovnistvo_names <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovnistvo.xlsx", sheetName = "7.4.2.",
                                startRow=7, encoding = "UTF-8", endRow=8,
                                colIndex = seq(1,20),
                                stringsAsFactors=F))

stanovnistvo_doseljeno_iseljeno <- list()
stanovnistvo_doseljeno_iseljeno <- lapply(seq(8, 197, 9), function(i){
  read.xlsx("./data/statistika u nizu/Stanovnistvo.xlsx", sheetName = "7.4.2.", 
            startRow=i, encoding = "UTF-8", endRow=i+7, 
            colIndex = seq(1,20), 
            stringsAsFactors=F)
})

st_saldo <- stanovnistvo_names[F,]
st_doseljeni <- stanovnistvo_names[F,]
st_doseljeni_druga_zup <- stanovnistvo_names[F,]
st_doseljeni_druga_drz <- stanovnistvo_names[F,]
st_odseljeni <- stanovnistvo_names[F,]
st_odseljeni_druga_zup <- stanovnistvo_names[F,]
st_odseljeni_druga_drz <- stanovnistvo_names[F,]

for(i in seq(1,22)){
  pom <- ocisti_dataframe(data.frame(stanovnistvo_doseljeno_iseljeno[i]))
  
  #kako bi pokupili naziv zupanije u svaki redak
  for(j in seq(1,7))    pom[j,][1:2] <- trimws(gsub(".", " ",names(pom)[1:2], fixed = TRUE))
  
  #nedostaju - u imenima zupanija (jer su bili u names pa im je R zamijenio s .), no grad zagreb i RH ne trebaju -
  if(pom[,1][1] != "Grad Zagreb" && pom[,1][1] != "Republika Hrvatska")
  {
    pom[,1] <- gsub(" ", "-",pom[,1])
  }

  #imena moraju biti konzistentna kako bi se moglo dodati na kraj dataframea
  names(pom) <- names(stanovnistvo_names)
  st_saldo <- bind_rows(st_saldo, pom[1,])
  st_doseljeni <- bind_rows(st_doseljeni, pom[2,])
  st_doseljeni_druga_zup <- bind_rows(st_doseljeni_druga_zup, pom[3,])
  st_doseljeni_druga_drz <- bind_rows(st_doseljeni_druga_drz, pom[4,])
  st_odseljeni <- bind_rows(st_odseljeni, pom[5,])
  st_odseljeni_druga_zup <- bind_rows(st_odseljeni_druga_zup, pom[6,])
  st_odseljeni_druga_drz <- bind_rows(st_odseljeni_druga_drz, pom[7,])
}
rm(pom)

write.csv((st_saldo), "./shiny-app/data/stanovnistvo saldo migracije s inozemstvom.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_doseljeni), "./shiny-app/data/stanovnistvo broj doseljenih.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_doseljeni_druga_zup), "./shiny-app/data/stanovnistvo broj doseljenih iz druge zupanije.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_doseljeni_druga_drz), "./shiny-app/data/stanovnistvo broj doseljenih iz druge drzave.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_odseljeni), "./shiny-app/data/stanovnistvo broj odseljenih.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_odseljeni_druga_zup), "./shiny-app/data/stanovnistvo broj odseljenih u drugu zupaniju.csv", row.names=FALSE, fileEncoding = "UTF-8")
write.csv((st_odseljeni_druga_drz), "./shiny-app/data/stanovnistvo broj odseljenih u drugu drzavu.csv", row.names=FALSE, fileEncoding = "UTF-8")

##7.2.3. PROCJENA UKUPNOG BROJA STANOVNIKA SREDINOM GODINE
stanovnistvo_procjena <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovnistvo.xlsx", sheetName = "7.4.3.",
                                startRow=7, encoding = "UTF-8", endRow=29,
                                colIndex = seq(1,17),
                                stringsAsFactors=F))

names(stanovnistvo_procjena)[3:17] <- gsub('.{2}$', '', names(stanovnistvo_procjena)[3:17] ) #micanje anotacija s godina
write.csv((stanovnistvo_procjena), "./shiny-app/data/stanovnistvo procjena br st sredinom godine.csv", row.names=FALSE, fileEncoding = "UTF-8")


# Transport.xlsx #############################
tran_cestovna <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.1.",
                                   startRow=9, encoding = "UTF-8", endRow=32,
                                   colIndex = seq(1,13),
                                   stringsAsFactors=F)[-c(2),])
tran_cestovna[4,1] <- "Sisačko-moslavačka" #ispravljanje tipfelera
write.csv((tran_cestovna), "./shiny-app/data/transport ukupni broj cestovnih mreza.csv", row.names=FALSE, fileEncoding = "UTF-8")

#4
tran_sheet1 <-c("autocesta.csv", "drzavnih cesta.csv", "zupanijskih cesta.csv", "lokalnih cesta.csv")
tran_stupci <- c(15,27,39,51)
for(i in seq(1,4)){
  tran_ceste <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.1.",
                             startRow=9, encoding = "UTF-8", endRow=32,
                             colIndex = seq(tran_stupci[i],tran_stupci[i]+10), 
                             stringsAsFactors=F)[-c(2),]
  ##autoceste nemaju vrijednost u prva dva stupca, a R odbija prepoznati "..."
  if(i == 1){
    tran_ceste[1:2] <-NA
  }
  tran_ceste <- (ocisti_dataframe(cbind(tran_cestovna[,1:2], tran_ceste)))
  write.csv(tran_ceste, paste("./shiny-app/data/transport ukupni broj", tran_sheet1[i]), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(tran_ceste)

tran_gustoca <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.2.",
                                            startRow=8, encoding = "UTF-8", endRow=31,
                                            colIndex = seq(1,13),
                                            stringsAsFactors=F)[-c(2),])
tran_gustoca[4,1] <- "Sisačko-moslavačka" #ispravljanje tipfelera
write.csv(tran_gustoca, "./shiny-app/data/transport gustoca cestovne mreze.csv", row.names=FALSE, fileEncoding = "UTF-8")

tran_prijevoz_robe <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.3.",
                                           startRow=9, encoding = "UTF-8", endRow=32,
                                           colIndex = seq(1,17),
                                           stringsAsFactors=F)[-c(2),])
tran_prijevoz_robe[4,1] <- "Sisačko-moslavačka" #ispravljanje tipfelera
write.csv(tran_prijevoz_robe, "./shiny-app/data/transport cestovni prijevoz robe.csv", row.names=FALSE, fileEncoding = "UTF-8")

tran_nesrece_imena <- c("ukupno.csv", "s poginulim osobama.csv", "s ozlijedenim osobama.csv")
tran_sheetovi <- c("5.4.4.","5.4.5.")

#mora biti zasebna funkcija jer su policijske uprave a ne zupanije
tran_translacija <- function(df){
  zupanije <- df$Policijska.uprava
  
  df <- as.data.frame(t(df[,-(1:2)]))
  colnames(df) <- zupanije
  rownames(df)<- gsub('X', '',rownames(df))
  df
}

for (j in c(1,2)){
  for(i in seq(1,3)){
    tran_nesrece <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = tran_sheetovi[j],
                                                     startRow=7, encoding = "UTF-8", endRow=29,
                                                     colIndex = seq( ifelse(i==1,1, ifelse(i==2,17,31)), ifelse(i==1,15, ifelse(i==2,29,43))),
                                                     stringsAsFactors=F)
    tran_nesrece <- tran_nesrece[-c(2),]
    if(i== 1){
      tran_nesrece$Policijska.uprava[2] <- "Zagrebačka"
      tran_nesrece$Policijska.uprava[4] <- "Sisačko-moslavačka" #ispravljanje tipfelera
      tran_zup <- tran_nesrece[,1:2]
    }else{
      tran_nesrece <- cbind(tran_zup, tran_nesrece)
    }
    tran_nesrece <- ocisti_dataframe(tran_nesrece)
    write.csv(tran_nesrece, paste("./shiny-app/data/transport broj",ifelse(j==1, "prometnih nesreca", "nastradalih u prometu"), tran_nesrece_imena[i]), row.names=FALSE, fileEncoding = "UTF-8")
  }
}
rm(tran_zup)
rm(tran_nesrece)

##TODO: 5.4.6. i 5.4.7. zracne luke, 5.4.8. registrirana vozila i 5.4.9. zeljeznicki promet






# Turizam.xlsx #############################
#4.3.2.1. DOLASCI TURISTA U KOMERCIJALNIM SMJEsTAJNIM OBJEKTIMA
tur_sheet <- c("4.3.2.1.", "4.3.2.2.")
tur_imena <- c("ukupno.csv", "domaci.csv", "strani.csv")
for(j in c(1,2)){
  for(i in seq(1,3)){
    if(j == 2 && i == 3) break()
    turizam_dolasci <- read.xlsx("./data/statistika u nizu/Turizam.xlsx", sheetName = tur_sheet[j],
                                                     startRow=7, encoding = "UTF-8", endRow=30,
                                                     colIndex = seq(ifelse(i==1, 1, ifelse(i==2, 27, 51)),
                                                                    ifelse(i==1, 25, ifelse(i==2, 49, 73))),
                                                     stringsAsFactors=F)
    turizam_dolasci <- turizam_dolasci[-c(2),]
    #Anotacije na 14. i 16. retku od naziva zupanija
    if(i == 1){
      turizam_dolasci[14,1:2] <- gsub("2)","",turizam_dolasci[14,1:2], fixed = T )
      turizam_dolasci[16,1:2] <- gsub("2)","",turizam_dolasci[16,1:2], fixed = T )
      tur_zup <- turizam_dolasci[,1:2]
      names(turizam_dolasci) <- gsub(".1.","",names(turizam_dolasci), fixed = T )#anotacije na godinama 2001-2009
      tur_nam <- names(turizam_dolasci)
    }else{
      turizam_dolasci <- cbind(tur_zup, turizam_dolasci)
      names(turizam_dolasci) <- tur_nam
    }
    turizam_dolasci <- (ocisti_dataframe(turizam_dolasci))
    write.csv(turizam_dolasci, paste("./shiny-app/data/turizam broj",ifelse(j==1, "dolazaka", "nocenja"), ifelse(i == 2 && j==2, tur_imena[i+1], tur_imena[i])), row.names=FALSE, fileEncoding = "UTF-8")
  }
}
rm(tur_nam)
rm(tur_zup)
rm(turizam_dolasci)

#4.3.2.3. LUKE NAUTIcKOG TURIZMA
tur_imena <- c("luka u nautickom turizmu.csv", "plovila na stalnom vezu u lukama.csv", "plovila u tranzitu u lukama.csv")
for(i in seq(1,3)){
  tur_luke <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Turizam.xlsx", sheetName = "4.3.2.3.",
                            startRow=7, encoding = "UTF-8", endRow=14,
                            colIndex = seq( ifelse(i==1,1, ifelse(i==2,16,29)), ifelse(i==1,14, ifelse(i==2,27,40))),
                            stringsAsFactors=F))
  if(i!= 1){
    tur_luke <- cbind(tur_zup, tur_luke)
  }else{
    tur_zup <- tur_luke[,1:2]
  }
  write.csv((tur_luke), paste("./shiny-app/data/turizam broj", tur_imena[i]), row.names=FALSE, fileEncoding = "UTF-8")
}
rm(tur_zup)
rm(tur_luke)

# Zaposlenost i place.xlsx #############################
#faith in humanity restored!
zaposlenost <- function(sheet, br_podataka, imena, pocetni_red, zadnji_stupac){
  zap1 <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Zaposlenost i place.xlsx", sheetName = sheet,
                                    startRow=pocetni_red, encoding = "UTF-8", endRow=pocetni_red+22,
                                    colIndex = seq(1,zadnji_stupac), stringsAsFactors=F))
  
  zap1[4,1] <- "Sisačko-moslavačka" #ispravljanje tipfelera
  write.csv((zap1), paste("./shiny-app/data/zaposlenost ", imena[1], ".csv", sep=""), row.names=FALSE, fileEncoding = "UTF-8")
  
  
  if(br_podataka > 1){
    zap2 <- read.xlsx("./data/statistika u nizu/Zaposlenost i place.xlsx", sheetName = sheet,
                                       startRow=7, encoding = "UTF-8", endRow=29,
                                       colIndex = seq(25,45), stringsAsFactors=F)
    zap2 <- ocisti_dataframe(cbind(zap1[,1:2], zap2))
    names(zap2) <- names(zap1)
    write.csv((zap2), paste("./shiny-app/data/zaposlenost ", imena[2], ".csv", sep=""), row.names=FALSE, fileEncoding = "UTF-8")
  }
}

zaposlenost("9.2.1.", 2, c("neto place", "bruto place"), 7, 23)
zaposlenost("9.2.2.", 1, c("stopa registrirane nezaposlenosti"),8, 19)
zaposlenost("9.2.3.", 1, c("nezaposleni"),7,19)
zaposlenost("9.2.4.", 1, c("aktivni osiguranici-individualni poljoprivrednici"),7,19)
zaposlenost("9.2.5.", 1, c("zaposleni u obrtu i slobodnim profesijama"),7,19)
zaposlenost("9.2.6.", 1, c("zaposleni u pravnim osobama"),7,19)
zaposlenost("9.2.7.", 1, c("aktivno stanovnistvo"),7,19)
zaposlenost("9.2.8.", 1, c("ukupno zaposleni"),7,19)


#stvaranje "relativnih" podataka za tablice ####
procjena_godine <- names(stanovnistvo_procjena)
stanovnistvo_procjena <- stanovnistvo_procjena[order(stanovnistvo_procjena$Županija),]
rownames(stanovnistvo_procjena) <- NULL

#BDP ne treba jer ima i po stanovniku podatke

pripremi_dataframe <- function(df){
  names(df)[1] <- "Županija"
  df <- df[order(df$Županija),]
  df <- df[!(df$Županija == "Neraspoređeno" | df$Županija == "Slobodne zone" | df$Županija == "Ukupno" | df$Županija == "Nepoznato"),]
  names(df) <- gsub("X", "", names(df))
  rownames(df) <- NULL
  subset(df, select = names(df) %in% procjena_godine)
}

iteriraj_po_setu<- function(naziv){
  source_files <-list.files(path = "./shiny-app/data", pattern = paste("(",naziv ,") .*.csv", sep = ""), all.files = T, full.names = T, recursive = F)
  for(file in source_files){
    rel_podaci <- pripremi_dataframe(read.csv(file, stringsAsFactors = F, encoding = "UTF-8"))
    tryCatch({
    pom_stanovnici <- subset(stanovnistvo_procjena, select = names(stanovnistvo_procjena) %in% names(rel_podaci))
    pom_stanovnici <- pom_stanovnici[(pom_stanovnici$Županija %in% rel_podaci$Županija),]
    rownames(pom_stanovnici) <- NULL
    rel_podaci <- cbind(rel_podaci[1:2],round(rel_podaci[-(1:2)]/pom_stanovnici[-(1:2)],6))
    
    write.csv(rel_podaci, gsub("./shiny-app/data/", "./shiny-app/data/relativno/", file), row.names=FALSE, fileEncoding = "UTF-8")
    }, error = function(e){})
  }
}

for(item in c("grad", "kultura", "obrazovanje", "transport", "turizam", "zaposlenost", "zastita", "roba", "stanovnistvo", "industrija")){
  iteriraj_po_setu(item)
}
