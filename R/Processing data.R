install.packages('xlsx')
require(xlsx)

# Robna razmjena s inozemstvom.xlsx ############################# 

izvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                   startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,19))
names(izvoz)
# ako želimo ukloniti samo  prva dva prazna retka, izvoz[-seq(1,3), ] ako želimo ukloniti i podatke za cijelu državu
izvoz <- izvoz[-c(1,2), ]
names(izvoz)[1] <- "Županija"
names(izvoz)[2] <- "County of"
#u xlsx fileu 2016 ima footnote koji se u Ru ispisuje kao "X2016.1............."
names(izvoz)[19] <- "X2016."
uvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                  startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,19))
head(uvoz)
names(uvoz) <- names(izvoz)

#resetiranje brojeva redaka
rownames(izvoz) <- NULL
rownames(uvoz) <- NULL

write.csv(izvoz, "./data/processed data/izvoz u tisucama kuna.csv")
write.csv(uvoz, "./data/processed data/uvoz u tisucama kuna.csv")

izvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                       startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,19))
izvozEuri <- izvozEuri[-c(1,2), ]
names(izvozEuri)[1] <- "Županija"
names(izvozEuri)[2] <- "County of"
names(izvozEuri)[9] <- "X2016."

uvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                      startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,19))
names(uvozEuri) <- names(izvozEuri)

rownames(izvozEuri) <- NULL
rownames(uvozEuri) <- NULL
write.csv(izvoz, "./data/processed data/izvoz u tisucama eura.csv")
write.csv(uvoz, "./data/processed data/uvoz u tisucama eura.csv")




# Bruto domaci proizvod.xlsx ############################# 

bdp <- read.xlsx("./data/statistika u nizu/Bruto domaci proizvod.xlsx", sheetName ="12.1.2.1.", 
                 startRow=7, encoding = "UTF-8", endRow=33, colIndex = seq(1,17), stringsAsFactors=F)
#retci 3 i 19 imaju podatke za kontinentalnu /Jadransku hrvatsku, 
#to ćemo ukloniti kako bi sačuvali uniformnost podataka, retci 2 i 18 su prazni

bdp <- bdp[-c(2,3,18,19), ]
names(bdp)[1] <- "Županija"
names(bdp)[2] <- "County of"
rownames(bdp) <- NULL
write.csv(bdp, "./data/processed data/bdp u tisucama kuna.csv")

bdpEuri <- read.xlsx("./data/statistika u nizu/Bruto domaci proizvod.xlsx", sheetName ="12.1.2.1.", 
                     startRow=7, encoding = "UTF-8", endRow=33, colIndex = seq(19,33), stringsAsFactors=F)

bdpEuri <- bdpEuri[-c(2,3,18,19), ]
rownames(bdpEuri) <- NULL
#ovdje su podaci razdijeljeni okomito a ne vodoravno kao u prethodnoj tablici
#zato nedostaju prva dva stupca s nazivima županija (stoga ćemo ih preuzeti iz prvog seta)
bdpEuri <- cbind(bdp[,1:2], bdpEuri)
write.csv(bdpEuri, "./data/processed data/bdp u tisucama eura.csv")

for(i in c(1,2)){
  bdpPoStanovniku <- read.xlsx("./data/statistika u nizu/Bruto domaci proizvod.xlsx", sheetName ="12.1.2.2.", 
                               startRow=7, encoding = "UTF-8", endRow=33, 
                               colIndex = seq(ifelse(i==1, 1, 19), ifelse(i==1, 17, 33)), stringsAsFactors=F)
  
  bdpPoStanovniku <- bdpPoStanovniku[-c(2,3,18,19), ]
  if(i==1){
    bdpPoStanovniku[1,1:2] <-bdp[1,1:2]
  }else{
    bdpPoStanovniku <- cbind(bdp[,1:2], bdpPoStanovniku)
  }
  names(bdpPoStanovniku)<- names(bdp)
  rownames(bdpPoStanovniku) <- NULL
  write.csv(bdpPoStanovniku, paste("./data/processed data/bdp po stanovniku u tisucama", ifelse(i==1, "kuna.csv", "eura.csv")))
}





# Građevinarstvo.xlsx ############################# 

ukupno_izdanih_dozvola <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.1.", 
                                    startRow=7, encoding = "UTF-8", endRow=30, 
                                    colIndex = seq(1,16), stringsAsFactors=F)
#uredni podaci :O
#višak je "županija" nakon naziva svake županije
#zadnji red umjesto "Bez lokacije županije2)"	Unclassified2) popraviti na "Neraspoređeno" radi konzistentnosti

ukupno_izdanih_dozvola[23,1] <- "Neraspoređeno"
ukupno_izdanih_dozvola[23,2] <- "Unclassified"
ukupno_izdanih_dozvola[,1] <- gsub(" županija","",ukupno_izdanih_dozvola[,1])

write.csv(ukupno_izdanih_dozvola, "./data/processed data/građ ukupno izdanih građevinskih dozvola.csv")

gradevina_stupci <- c(1, 18, 33, 48, 63, 78, 93)
gradevina_prvi_sheet <- c("za zgrade.csv", "za ostale građevine.csv")
grad_zupanije_Unclassified <- ukupno_izdanih_dozvola[,1:2]

for(i in c(1,2)){
  sobni_stanovi <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.1.", 
                             startRow=7, encoding = "UTF-8", endRow=30, 
                             colIndex = seq(gradevina_stupci[i+1], gradevina_stupci[i+1]+ 13), 
                             stringsAsFactors=F)
  
  sobni_stanovi <- cbind(grad_zupanije_Unclassified, sobni_stanovi)
  write.csv(sobni_stanovi, paste("./data/processed data/građ izdane građevinske dozvole", gradevina_prvi_sheet[i]))
}

gradevina_zupanije

##3.2.2. - GRAĐEVINSKE VELIČINE ZGRADA ZA KOJE SU IZDANE GRAĐEVINSKE DOZVOLE (novogradnja i dogradnja)
gradevina_drugi_sheet <- c("ukupna veličina", "veličina stambenih", "veličina nestambenih")

for(i in c(1,2,3)){
  sobni_stanovi <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.2.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], ifelse(i==1, gradevina_stupci[i]+ 15, gradevina_stupci[i]+ 13)),
                             stringsAsFactors=F)
  if(i != 1){
    sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  }else{
    sobni_stanovi[,1] <- gsub(" županija","",sobni_stanovi[,1])
    gradevina_zupanije <- sobni_stanovi[,1:2]
  }
  write.csv(sobni_stanovi, paste("./data/processed data/građ", gradevina_drugi_sheet[i], "zgrada za koje su izdane građevinske dozvole.csv"))
}


##3.2.3. - BROJ I POVRŠINA STANOVA ZA KOJE SU IZDANE GRAĐEVINSKE DOZVOLE
gradevina_treci_sheet <- c("broj stanova", "korisna površina m2 stanova")
for(i in c(1,2)){
  sobni_stanovi <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.3.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], gradevina_stupci[i]+ 13), 
                             stringsAsFactors=F)
  
  sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  write.csv(sobni_stanovi, paste("./data/processed data/građ", gradevina_treci_sheet[i], "za koje su izdane građevinske dozvole.csv"))
}

##3.2.4. - BROJ I GRAĐEVINSKE VELIČINE ZAVRŠENIH ZGRADA

ukupno_velicina_zavrsenih_zgrada <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.4.", startRow=7, encoding = "UTF-8", endRow=30, colIndex = seq(1,30), stringsAsFactors=F)
names(ukupno_velicina_zavrsenih_zgrada)
##header je rascjepkan kroz dva retka
##		                2002.		2003.		2004.		2005.		2006.		2007.		2008.		2009.		2010.		2011.		2012.		2013.		2014.		2015.	
#Županija	County of	"broj Number"	"površina, m2 Floor area, m2"	...
names(ukupno_velicina_zavrsenih_zgrada)[1] <- ukupno_velicina_zavrsenih_zgrada[1,1]
names(ukupno_velicina_zavrsenih_zgrada)[2] <- ukupno_velicina_zavrsenih_zgrada[1,2]
#namještanje headera da liči na nešto
names(ukupno_velicina_zavrsenih_zgrada)[c(FALSE, TRUE)][-1] <- paste(substr(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1],1,6), "površina, m2")
names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1] <- paste(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1], "broj")

names(ukupno_velicina_zavrsenih_zgrada)
ukupno_velicina_zavrsenih_zgrada <- ukupno_velicina_zavrsenih_zgrada[-c(1), ]
rownames(ukupno_velicina_zavrsenih_zgrada) <- NULL
ukupno_velicina_zavrsenih_zgrada[,1] <- gradevina_zupanije[,1] #uklanjanje županija s kraja imena

write.csv(ukupno_velicina_zavrsenih_zgrada, paste("./data/processed data/građ ukupna veličina završenih zgrada za koje su izdane građevinske dozvole.csv"))

for(i in c(1,2)){
  vel_zgrada <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.4.", 
                          startRow=9, encoding = "UTF-8", endRow=30, 
                          colIndex = seq(ifelse(i==1, 32, 61),ifelse(i==1, 59, 88)), 
                          stringsAsFactors=F, header = F)
  
  vel_zgrada <- cbind(ukupno_velicina_zavrsenih_zgrada[,1:2], vel_zgrada)
  names(vel_zgrada) <- names(ukupno_velicina_zavrsenih_zgrada)
  write.csv(vel_zgrada, paste("./data/processed data/građ", gradevina_drugi_sheet[i+1], "(završenih) zgrada za koje su izdane građevinske dozvole.csv"))
}

## 3.2.5. - ZAVRŠENI STANOVI 
zavrseni_stanovi <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.5.", 
                              startRow=8, encoding = "UTF-8", endRow=30, 
                              colIndex = seq(1,30), stringsAsFactors=F)

names(zavrseni_stanovi) <- names(ukupno_velicina_zavrsenih_zgrada)
zavrseni_stanovi[,1] <- gradevina_zupanije[,1]
write.csv(zavrseni_stanovi, paste("./data/processed data/građ ukupni broj završenih stanova za koje su izdane građevinske dozvole.csv"))


## 3.2.6. - ZAVRŠENI STANOVI PREMA BROJU SOBA

grad_fja_obrada <- function(brojIteracija, sheet, pocetniRed, krajnjiRed, zupanija, sheetImena){
  for(i in brojIteracija){
    sobni_stanovi <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName =sheet, 
                               startRow=pocetniRed, encoding = "UTF-8", endRow=krajnjiRed, 
                               colIndex = seq(gradevina_stupci[i], ifelse(i==1, gradevina_stupci[i]+ 15, gradevina_stupci[i]+ 13)), stringsAsFactors=F)
    if(i != 1){
      sobni_stanovi <- cbind(zupanija, sobni_stanovi)
    }else{
      sobni_stanovi[,1] <- zupanija[,1]
    }
    write.csv(sobni_stanovi, paste("./data/processed data/građ", sheetImena[i], "za koje su izdane građevinske dozvole.csv"))
  }
}

gradevina_sesti_sheet <- c("1-sobni završeni stanovi", "2-sobni završeni stanovi", "3-sobni završeni stanovi", "4-sobni završeni stanovi", "5-sobni i višesobni završeni stanovi")
grad_fja_obrada(seq(1,5), "3.2.6.", 7, 29, gradevina_zupanije, gradevina_sesti_sheet)


## 3.2.7. - VRIJEDNOST IZVRŠENIH GRAĐEVINSKIH RADOVA PREMA VRSTI GRAĐEVINA
#iste županije kao u 1. (ima unclassified na dnu)
#godine 2002-2015, okomito odvojenih setova podataka: 7
#Ukupno, Stambene zgrade, Nestambene zgrade, Prometna infrakstruktura, Cjevovodi, komunikacijski i električni vodovi, Složene građevine na industrijskim prostorima, Ostale nespomenute građevine
gradevina_sedmi_sheet <- c("ukupna", "stambene zgrade", "nestambene zgrade", "prometna infrakstruktura", "cjevovodi, komunikacijski i električni vodovi", "složene građevine na industrijskim prostorima", "ostale nespomenute građevine")
grad_fja_obrada(seq(1,7), "3.2.7.", 7, 30, grad_zupanije_Unclassified, gradevina_sedmi_sheet)

# Industrija.xlsx ############################# 
industrija <- read.xlsx("./data/statistika u nizu/Industrija.xlsx", sheetName ="2.1.2.1.", 
                           startRow=9, encoding = "UTF-8", endRow=32, 
                           colIndex = seq(1,15), 
                           stringsAsFactors=F
)
write.csv(industrija, "./data/processed data/industrija ukupna vrijednost prodanih proizvoda po NP-u.csv")


# Kultura.xlsx ############################# 
kultura_sheetovi <- c("8.3.1.","8.3.2.","8.3.3.","8.3.4.","8.3.5.")
kultura_podaci <- c("kina", "sjedala u kinima", "posjetitelja u kinima", "radiopretplatnika", "tv pretplatnika")

for(i in seq(1,5)){
  kultura <- read.xlsx("./data/statistika u nizu/Kultura.xlsx", sheetName =kultura_sheetovi[i], 
                          startRow=7, encoding = "UTF-8", endRow=29, 
                          colIndex = seq(1,25), 
                          stringsAsFactors=F
  )
  write.csv(kultura, paste("./data/processed data/kultura broj", kultura_podaci[i], ".csv"))
}

# Obrazovanje.xlsx ############################# 
#prva tri sheeta - broj ustanova i broj djece okomito odvojeni, županije normalno raspisane
#zadnja dva ukupno i redovni 1.4. -akademske godine, 1.5. - obične godine
obrazovanje_sheetovi_1 <- c("8.1.1.", "8.1.2.", "8.1.3.")
obrazovanje_podaci_1 <- c("dječji vrtići", "osnovne škole", "srednje škole")

#### trimws(x) ---> uklanja leading & trailing spaces

for(i in c(1,2,3)){
  obrazovanje_ustanove <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                           startRow=7, encoding = "UTF-8", endRow=29, 
                           colIndex = seq(1,13), 
                           stringsAsFactors=F)
  
  obrazovanje_djeca <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                            startRow=7, encoding = "UTF-8", endRow=29, 
                            colIndex = seq(15,25), 
                            stringsAsFactors=F)
  obrazovanje_ustanove[obrazovanje_ustanove== "-"] <- NA
  obrazovanje_djeca[obrazovanje_djeca== "-"] <- NA
  obrazovanje_ustanove[,1] <- trimws(obrazovanje_ustanove[,1])
  obrazovanje_ustanove[,2] <- trimws(obrazovanje_ustanove[,2])
  obrazovanje_djeca <- cbind(obrazovanje_ustanove[,1:2], obrazovanje_djeca)
  write.csv(obrazovanje_ustanove, paste("./data/processed data/obrazovanje", obrazovanje_podaci_1[i],"broj ustanova.csv"))
  write.csv(obrazovanje_djeca, paste("./data/processed data/obrazovanje", obrazovanje_podaci_1[i], "broj djece.csv"))
}

obrazovanje_sheetovi_2 <- c("8.1.4.", "8.1.5.")
obrazovanje_podaci_2 <- c("upisanih studenata.csv", "studenata koji su završili.csv")
for(i in c(1,2)){
  obrazovanje_ukupno <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(1,ifelse(i==1,22,23)), 
                                  stringsAsFactors=F)
  
  obrazovanje_redovni <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(ifelse(i==1, 24, 25),ifelse(i==1,43,45)), 
                                  stringsAsFactors=F)
  obrazovanje_ustanove[obrazovanje_ustanove== "-"] <- NA
  obrazovanje_djeca[obrazovanje_djeca== "-"] <- NA
  obrazovanje_ukupno[,1] <- trimws(obrazovanje_ukupno[,1])
  obrazovanje_ukupno[,2] <- trimws(obrazovanje_ukupno[,2])
  obrazovanje_redovni <- cbind(obrazovanje_ukupno[,1:2], obrazovanje_redovni)
  write.csv(obrazovanje_ukupno, paste("./data/processed data/obrazovanje ukupan broj", obrazovanje_podaci_2[i]))
  write.csv(obrazovanje_redovni, paste("./data/processed data/obrazovanje broj redovnih", obrazovanje_podaci_2[i]))
}



# Okoliš.xlsx ############################# 
#3 seta okomito odvojenih podataka
okolis_stupci <- c(1,12,21)
okolis_imena <- c("tekući izdaci.csv", "prihodi.csv", "investicije.csv")

for(i in c(1,2,3)){
  okolis <- read.xlsx("./data/statistika u nizu/Okolis.xlsx", sheetName ="6.1.1.", 
                      startRow=9, encoding = "UTF-8", endRow=32, 
                      colIndex = seq(okolis_stupci[i], ifelse(i==1, okolis_stupci[i]+ 9, okolis_stupci[i]+ 7)), 
                      stringsAsFactors=F)
  okolis <- okolis[-c(2),]
  rownames(okolis) <- NULL
  if(i != 1){
    okolis <- cbind(okolis_zupanije, okolis)
  }else{
    okolis[,1]<- trimws(okolis[,1])
    okolis[,2]<- trimws(okolis[,2])
    okolis_zupanije <- okolis[,1:2]
  }
  names(okolis)[10] <- "X2015" #micanje anotacije
  write.csv(okolis, paste("./data/processed data/zaštita okoliša", okolis_imena[i]))
}


# Stanovništvo.xlsx #############################
#jedini odmah iskoristivi skup podataka je na 7.2.3. sheetu, ovo ostalo će potrajati malo

#RH 8-13, Zagrebačka  15-20, Krapinska 22-27... Zg 155-160
st_broj_zupanija <- seq(8, 155, 7)

#trebaju nam headeri
stanovnistvo_names <- read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.1.",
          startRow=7, encoding = "UTF-8", endRow=8,
          colIndex = seq(1,20),
          stringsAsFactors=F)

#lista, svaki član liste je ujedno dataframe sa podacima za pojedinu županiju
stanovnistvo_prirodno_kretanje <- list()
stanovnistvo_prirodno_kretanje <- lapply(st_broj_zupanija, function(i){
  read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.1.", 
            startRow=i, encoding = "UTF-8", endRow=i+5, 
            colIndex = seq(1,20), 
            stringsAsFactors=F)
})

head(stanovnistvo_prirodno_kretanje)
names(data.frame(stanovnistvo_prirodno_kretanje[22]))
st_broj_redaka <- seq(1,5)


library(dplyr)
#za svaki set podataka definiramo zasebni dataframe, stavljamo prazan al sa imenima koja nam trebaju
st_zivorodeni <- stanovnistvo_names[F,]
st_umrli <- stanovnistvo_names[F,]
st_prirast <- stanovnistvo_names[F,]
st_sklopljeni_brakovi <- stanovnistvo_names[F,]
st_razvedeni_brakovi <- stanovnistvo_names[F,]


for(i in seq(1,22)){
  pom <- data.frame(stanovnistvo_prirodno_kretanje[i])
  #kako bi pokupili naziv županije u svaki redak
  for(j in st_broj_redaka)    pom[j,][1:2] <- names(pom)[1:2]
  #imena moraju biti konzistentna kako bi se moglo dodati na kraj dataframea
  names(pom) <- names(stanovnistvo_names)
  st_zivorodeni <- bind_rows(st_zivorodeni, pom[1,])
  st_umrli <- bind_rows(st_umrli, pom[2,])
  st_prirast <- bind_rows(st_prirast, pom[3,])
  st_sklopljeni_brakovi <- bind_rows(st_sklopljeni_brakovi, pom[4,])
  st_razvedeni_brakovi <- bind_rows(st_razvedeni_brakovi, pom[5,])
}

write.csv(st_zivorodeni, "./data/processed data/stanovništvo broj živorođenih.csv")
write.csv(st_umrli, "./data/processed data/stanovništvo broj umrlih.csv")
write.csv(st_prirast, "./data/processed data/stanovništvo prirodni prirast.csv")
write.csv(st_sklopljeni_brakovi, "./data/processed data/stanovništvo broj sklopljenih brakova.csv")
write.csv(st_razvedeni_brakovi, "./data/processed data/stanovništvo broj razvedenih brakova.csv")



