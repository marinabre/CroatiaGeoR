install.packages('xlsx')
library(xlsx)
require(xlsx)

#### trimws(x) ---> uklanja leading & trailing spaces
ocisti_dataframe <- function(df,br_stupaca){
  df[df == "-"] <- NA
  df[,3:br_stupaca] <- sapply(df[,3:br_stupaca], function(x){as.numeric(gsub(' ', '',x))})
  df[,1] <- trimws(gsub(" županija","",df[,1]))
  df[,2] <- trimws(df[,2])
  names(df) <- gsub('X', '',names(df))
  rownames(df) <- NULL
  df
}


# Robna razmjena s inozemstvom.xlsx ############################# 

izvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                   startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,19))

# ako želimo ukloniti samo  prva dva prazna retka, izvoz[-seq(1,3), ] ako želimo ukloniti i podatke za cijelu državu
izvoz <- izvoz[-c(1,2), ]
names(izvoz)[1] <- "Županija"
names(izvoz)[2] <- "County.of"
#u xlsx fileu 2016 ima footnote koji se u Ru ispisuje kao "X2016.1............."
names(izvoz)[19] <- "X2016."
izvoz <- ocisti_dataframe(izvoz, 19)

uvoz <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.1.", 
                  startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,19))
names(uvoz) <- names(izvoz)
uvoz <- ocisti_dataframe(uvoz, 19)

write.csv(izvoz, "./data/processed data/izvoz u tisucama kuna.csv")
write.csv(uvoz, "./data/processed data/uvoz u tisucama kuna.csv")

izvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                       startRow=8, encoding = "UTF-8", endRow=33, colIndex = seq(1,9))

izvozEuri <- izvozEuri[-c(1,2), ]
names(izvozEuri)[1:2] <- names(izvoz)[1:2]
names(izvozEuri)[9] <- "X2016."
izvozEuri <- ocisti_dataframe(izvozEuri, 9)

uvozEuri <- read.xlsx("./data/statistika u nizu/Robna razmjena s inozemstvom.xlsx", sheetName ="4.2.3.2.", 
                      startRow=34, encoding = "UTF-8", endRow=57, colIndex = seq(1,9))

names(uvozEuri) <- names(izvozEuri)
uvozEuri <- ocisti_dataframe(uvozEuri, 9)

write.csv(izvoz, "./data/processed data/izvoz u tisucama eura.csv")
write.csv(uvoz, "./data/processed data/uvoz u tisucama eura.csv")




# Bruto domaci proizvod.xlsx ############################# 
for(j in c(1,2)){
  for(i in c(1,2)){
    bdp <- read.xlsx("./data/statistika u nizu/Bruto domaci proizvod.xlsx", sheetName =ifelse(j==1,"12.1.2.1.", "12.1.2.2."),
                     startRow=7, encoding = "UTF-8", endRow=33, colIndex = seq(ifelse(i==1,1,19),ifelse(i==1,17,33)), stringsAsFactors=F)
    #retci 3 i 19 imaju podatke za kontinentalnu /Jadransku hrvatsku, 
    #to ćemo ukloniti kako bi sačuvali uniformnost podataka, retci 2 i 18 su prazni
    bdp <- bdp[-c(2,3,18,19), ]
    if(i==1){
      names(bdp)[1:2] <- names(izvoz)[1:2]
      bdp_zup <- bdp[,1:2]
      bdp_names <- names(bdp)
    }else{
      bdp <- cbind(bdp_zup, bdp)
    }
    if(j==2) {
      bdp[,1:2] <- bdp_zup
      names(bdp) <- bdp_names
      }
    bdp <- ocisti_dataframe(bdp, 17)
    write.csv(bdp, paste("./data/processed data/bdp",ifelse(j==1,"","po stanovniku"),"u tisucama", ifelse(i==1, "kuna", "eura"), ".csv"))
  }
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
ukupno_izdanih_dozvola <- ocisti_dataframe(ukupno_izdanih_dozvola, 16)
write.csv(ukupno_izdanih_dozvola, "./data/processed data/građ ukupno izdanih građevinskih dozvola.csv")

gradevina_stupci <- c(1, 18, 33, 48, 63, 78, 93)
gradevina_prvi_sheet <- c("za zgrade.csv", "za ostale građevine.csv")
grad_zupanije_Unclassified <- ukupno_izdanih_dozvola[,1:2]

for(i in c(1,2)){
  sobni_stanovi <- ocisti_dataframe(
            read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.1.", 
                             startRow=7, encoding = "UTF-8", endRow=30, 
                             colIndex = seq(gradevina_stupci[i+1], gradevina_stupci[i+1]+ 13), 
                             stringsAsFactors=F), 14)
  
  sobni_stanovi <- cbind(grad_zupanije_Unclassified, sobni_stanovi)
  write.csv(sobni_stanovi, paste("./data/processed data/građ izdane građevinske dozvole", gradevina_prvi_sheet[i]))
}
rm(sobni_stanovi)

##3.2.2. - GRAĐEVINSKE VELIČINE ZGRADA ZA KOJE SU IZDANE GRAĐEVINSKE DOZVOLE (novogradnja i dogradnja)
gradevina_drugi_sheet <- c("ukupna veličina", "veličina stambenih", "veličina nestambenih")

for(i in c(1,2,3)){
  sobni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.2.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], ifelse(i==1, gradevina_stupci[i]+ 15, gradevina_stupci[i]+ 13)),
                             stringsAsFactors=F), ifelse(i==1, 16, 14))
  if(i != 1){
    sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  }else{
    gradevina_zupanije <- sobni_stanovi[,1:2]
  }
  write.csv(sobni_stanovi, paste("./data/processed data/građ", gradevina_drugi_sheet[i], "zgrada za koje su izdane građevinske dozvole.csv"))
}
rm(sobni_stanovi)

##3.2.3. - BROJ I POVRŠINA STANOVA ZA KOJE SU IZDANE GRAĐEVINSKE DOZVOLE
gradevina_treci_sheet <- c("broj stanova", "korisna površina m2 stanova")
for(i in c(1,2)){
  sobni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.3.", 
                             startRow=7, encoding = "UTF-8", endRow=29, 
                             colIndex = seq(gradevina_stupci[i], gradevina_stupci[i]+ 13), 
                             stringsAsFactors=F), 14)
  
  if(i !=1) sobni_stanovi <- cbind(gradevina_zupanije, sobni_stanovi)
  
  write.csv(sobni_stanovi, paste("./data/processed data/građ", gradevina_treci_sheet[i], "za koje su izdane građevinske dozvole.csv"))
}
rm(sobni_stanovi)
rm(gradevina_zupanije)
##3.2.4. - BROJ I GRAĐEVINSKE VELIČINE ZAVRŠENIH ZGRADA

ukupno_velicina_zavrsenih_zgrada <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.4.", 
                                              startRow=7, encoding = "UTF-8", endRow=30, 
                                              colIndex = seq(1,30), stringsAsFactors=F)
names(ukupno_velicina_zavrsenih_zgrada)
##header je rascjepkan kroz dva retka
##		                2002.		2003.		2004.		2005.		2006.		2007.		2008.		2009.		2010.		2011.		2012.		2013.		2014.		2015.	
#Županija	County of	"broj Number"	"površina, m2 Floor area, m2"	...
names(ukupno_velicina_zavrsenih_zgrada)[1] <- ukupno_velicina_zavrsenih_zgrada[1,1]
names(ukupno_velicina_zavrsenih_zgrada)[2] <- ukupno_velicina_zavrsenih_zgrada[1,2]
#namještanje headera da liči na nešto
names(ukupno_velicina_zavrsenih_zgrada)[c(FALSE, TRUE)][-1] <- paste(substr(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1],1,6), "površina, m2")
names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1] <- paste(names(ukupno_velicina_zavrsenih_zgrada)[c(TRUE, FALSE)][-1], "broj")

ukupno_velicina_zavrsenih_zgrada <- ukupno_velicina_zavrsenih_zgrada[-c(1), ]
ukupno_velicina_zavrsenih_zgrada <- ocisti_dataframe(ukupno_velicina_zavrsenih_zgrada, 30)

write.csv(ukupno_velicina_zavrsenih_zgrada, paste("./data/processed data/građ ukupna veličina završenih zgrada za koje su izdane građevinske dozvole.csv"))

for(i in c(1,2)){
  vel_zgrada <- read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.4.", 
                          startRow=9, encoding = "UTF-8", endRow=30, 
                          colIndex = seq(ifelse(i==1, 32, 61),ifelse(i==1, 59, 88)), 
                          stringsAsFactors=F, header = F)
  
  vel_zgrada <- cbind(ukupno_velicina_zavrsenih_zgrada[,1:2], vel_zgrada)
  names(vel_zgrada) <- names(ukupno_velicina_zavrsenih_zgrada)
  vel_zgrada <- ocisti_dataframe(vel_zgrada, 30)
  write.csv(vel_zgrada, paste("./data/processed data/građ", gradevina_drugi_sheet[i+1], "(završenih) zgrada za koje su izdane građevinske dozvole.csv"))
}
rm(vel_zgrada)
## 3.2.5. - ZAVRŠENI STANOVI 
zavrseni_stanovi <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Građevinarstvo.xlsx", sheetName ="3.2.5.", 
                              startRow=8, encoding = "UTF-8", endRow=30, 
                              colIndex = seq(1,30), stringsAsFactors=F), 30)

names(zavrseni_stanovi) <- names(ukupno_velicina_zavrsenih_zgrada)
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
    sobni_stanovi <- ocisti_dataframe(sobni_stanovi, 15)
    write.csv(sobni_stanovi, paste("./data/processed data/građ", sheetImena[i], "za koje su izdane građevinske dozvole.csv"))
  }
}
gradevina_sesti_sheet <- c("1-sobni završeni stanovi", "2-sobni završeni stanovi", "3-sobni završeni stanovi", "4-sobni završeni stanovi", "5-sobni i višesobni završeni stanovi")
grad_fja_obrada(seq(1,5), "3.2.6.", 7, 29, gradevina_zupanije, gradevina_sesti_sheet)


## 3.2.7. - VRIJEDNOST IZVRŠENIH GRAĐEVINSKIH RADOVA PREMA VRSTI GRAĐEVINA
#iste županije kao u 1. (ima unclassified na dnu)
#godine 2002-2015, okomito odvojenih setova podataka: 7
#Ukupno, Stambene zgrade, Nestambene zgrade, Prometna infrakstruktura, Cjevovodi, komunikacijski i električni vodovi, Složene građevine na industrijskim prostorima, Ostale nespomenute građevine
gradevina_sedmi_sheet <- c("ukupna vrijednost", "vrijednost stambenih zgrada", "vrijednost nestambenih zgrada", "vrijednost prometne infrakstrukture", "vrijednost cjevovoda, komunikacijskih i električnih vodova", "vrijednost složenih građevina na industrijskim prostorima", "vrijednost ostalih nespomenutih građevina")
grad_fja_obrada(seq(1,7), "3.2.7.", 7, 30, grad_zupanije_Unclassified, gradevina_sedmi_sheet)

# Industrija.xlsx ############################# 
industrija <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Industrija.xlsx", sheetName ="2.1.2.1.", 
                           startRow=9, encoding = "UTF-8", endRow=32, 
                           colIndex = seq(1,15), 
                           stringsAsFactors=F), 15)
write.csv(industrija, "./data/processed data/industrija ukupna vrijednost prodanih proizvoda po NP-u.csv")


# Kultura.xlsx ############################# 
kultura_sheetovi <- c("8.3.1.","8.3.2.","8.3.3.","8.3.4.","8.3.5.")
kultura_podaci <- c("kina", "sjedala u kinima", "posjetitelja u kinima", "radiopretplatnika", "tv pretplatnika")

for(i in seq(1,5)){
  kultura <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Kultura.xlsx", sheetName =kultura_sheetovi[i], 
                          startRow=7, encoding = "UTF-8", endRow=29, 
                          colIndex = seq(1,25), 
                          stringsAsFactors=F),25)
  if(i == 1)
    kultura_names <- names(kultura)
  names(kultura) <- kultura_names
  write.csv(kultura, paste("./data/processed data/kultura broj", kultura_podaci[i], ".csv"))
}
rm(kultura)
rm(kultura_names)
# Obrazovanje.xlsx ############################# 
#prva tri sheeta - broj ustanova i broj djece okomito odvojeni, županije normalno raspisane
#zadnja dva ukupno i redovni 1.4. -akademske godine, 1.5. - obične godine
obrazovanje_sheetovi_1 <- c("8.1.1.", "8.1.2.", "8.1.3.")
obrazovanje_podaci_1 <- c("dječji vrtići", "osnovne škole", "srednje škole")

for(i in c(1,2,3)){
  obrazovanje_ustanove <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                           startRow=7, encoding = "UTF-8", endRow=29, 
                           colIndex = seq(1,13), 
                           stringsAsFactors=F),13)
  
  obrazovanje_djeca <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_1[i], 
                            startRow=7, encoding = "UTF-8", endRow=29, 
                            colIndex = seq(15,25), 
                            stringsAsFactors=F)
  
  obrazovanje_djeca <- cbind(obrazovanje_ustanove[,1:2], obrazovanje_djeca)
  obrazovanje_djeca <- ocisti_dataframe(obrazovanje_djeca, 13)
  names(obrazovanje_djeca)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_djeca)[-(1:2)])
  names(obrazovanje_ustanove)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_ustanove)[-(1:2)])
  
  write.csv(obrazovanje_ustanove, paste("./data/processed data/obrazovanje", obrazovanje_podaci_1[i],"broj ustanova.csv"))
  write.csv(obrazovanje_djeca, paste("./data/processed data/obrazovanje", obrazovanje_podaci_1[i], "broj djece.csv"))
}
rm(obrazovanje_ustanove)
rm(obrazovanje_djeca)
obrazovanje_sheetovi_2 <- c("8.1.4.", "8.1.5.")
obrazovanje_podaci_2 <- c("upisanih studenata.csv", "studenata koji su završili.csv")
for(i in c(1,2)){
  obrazovanje_ukupno <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(1,ifelse(i==1,22,23)), 
                                  stringsAsFactors=F), ifelse(i==1,22,23))
  
  obrazovanje_redovni <- read.xlsx("./data/statistika u nizu/Obrazovanje.xlsx", sheetName = obrazovanje_sheetovi_2[i], 
                                  startRow=7, encoding = "UTF-8", endRow=30, 
                                  colIndex = seq(ifelse(i==1, 24, 25),ifelse(i==1,43,45)), 
                                  stringsAsFactors=F)
  
  obrazovanje_redovni <- cbind(obrazovanje_ukupno[,1:2], obrazovanje_redovni)
  obrazovanje_redovni <- ocisti_dataframe(obrazovanje_redovni, ifelse(i==1,22,23))
  if(i == 1)
  {#micanje akademskih godina, piše samo prva godina
    names(obrazovanje_ukupno)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_ukupno)[-(1:2)])
    names(obrazovanje_redovni)[-(1:2)] <- gsub('.{6}$', '', names(obrazovanje_redovni)[-(1:2)])
  }
  
  write.csv(obrazovanje_ukupno, paste("./data/processed data/obrazovanje ukupan broj", obrazovanje_podaci_2[i]))
  write.csv(obrazovanje_redovni, paste("./data/processed data/obrazovanje broj redovnih", obrazovanje_podaci_2[i]))
}
rm(obrazovanje_ukupno)
rm(obrazovanje_redovni)


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
  if(i != 1){
    okolis <- cbind(okolis_zupanije, okolis)
  }else{
    okolis_zupanije <- okolis[,1:2]
  }
  names(okolis)[10] <- "X2015." #micanje anotacije
  okolis <- ocisti_dataframe(okolis, 10)
  write.csv(okolis, paste("./data/processed data/zaštita okoliša", okolis_imena[i]))
}
rm(okolis)

# Stanovništvo.xlsx #############################
#jedini odmah iskoristivi skup podataka je na 7.2.3. sheetu, ovo ostalo će potrajati malo

#RH 8-13, Zagrebačka  15-20, Krapinska 22-27... Zg 155-160
st_broj_zupanija_prvi <- seq(8, 155, 7)

#trebaju nam headeri
stanovnistvo_names <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.1.",
          startRow=7, encoding = "UTF-8", endRow=8,
          colIndex = seq(1,20),
          stringsAsFactors=F),20)

#lista, svaki član liste je ujedno dataframe sa podacima za pojedinu županiju
stanovnistvo_prirodno_kretanje <- list()
stanovnistvo_prirodno_kretanje <- lapply(st_broj_zupanija_prvi, function(i){
  read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.1.", 
            startRow=i, encoding = "UTF-8", endRow=i+5, 
            colIndex = seq(1,20), 
            stringsAsFactors=F)
})

head(stanovnistvo_prirodno_kretanje)

library(dplyr)
#za svaki set podataka definiramo zasebni dataframe, stavljamo prazan al sa imenima koja nam trebaju
st_zivorodeni <- stanovnistvo_names[F,]
st_umrli <- stanovnistvo_names[F,]
st_prirast <- stanovnistvo_names[F,]
st_sklopljeni_brakovi <- stanovnistvo_names[F,]
st_razvedeni_brakovi <- stanovnistvo_names[F,]


for(i in seq(1,22)){
  pom <- ocisti_dataframe(data.frame(stanovnistvo_prirodno_kretanje[i]), 20)

  #kako bi pokupili naziv županije u svaki redak
  for(j in seq(1,5))    pom[j,][1:2] <- trimws(gsub(".", " ",names(pom)[1:2], fixed = TRUE))
  
  #nedostaju - u imenima županija (jer su bili u names pa im je R zamijenio s .), no grad zagreb i RH ne trebaju -
  pom[,1][2:5] <- gsub(" ", "-",pom[,1][2:5])
  
  #imena moraju biti konzistentna kako bi se moglo dodati na kraj dataframea
  names(pom) <- names(stanovnistvo_names)
  st_zivorodeni <- bind_rows(st_zivorodeni, pom[1,])
  st_umrli <- bind_rows(st_umrli, pom[2,])
  st_prirast <- bind_rows(st_prirast, pom[3,])
  st_sklopljeni_brakovi <- bind_rows(st_sklopljeni_brakovi, pom[4,])
  st_razvedeni_brakovi <- bind_rows(st_razvedeni_brakovi, pom[5,])
}
rm(pom)

write.csv(st_zivorodeni, "./data/processed data/stanovništvo broj živorođenih.csv")
write.csv(st_umrli, "./data/processed data/stanovništvo broj umrlih.csv")
write.csv(st_prirast, "./data/processed data/stanovništvo prirodni prirast.csv")
write.csv(st_sklopljeni_brakovi, "./data/processed data/stanovništvo broj sklopljenih brakova.csv")
write.csv(st_razvedeni_brakovi, "./data/processed data/stanovništvo broj razvedenih brakova.csv")

##7.2.2. DOSELJENO I ODSELJENO STANOVNIŠTVO
#taman kada pomisliš da je gornji sheet nešto najgore ikad...
stanovnistvo_names <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.2.",
                                startRow=7, encoding = "UTF-8", endRow=8,
                                colIndex = seq(1,20),
                                stringsAsFactors=F), 20)

stanovnistvo_doseljeno_iseljeno <- list()
stanovnistvo_doseljeno_iseljeno <- lapply(seq(8, 197, 9), function(i){
  read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.2.", 
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
  pom <- ocisti_dataframe(data.frame(stanovnistvo_doseljeno_iseljeno[i]), 20)
  
  #kako bi pokupili naziv županije u svaki redak
  for(j in seq(1,7))    pom[j,][1:2] <- trimws(gsub(".", " ",names(pom)[1:2], fixed = TRUE))
  
  #nedostaju - u imenima županija (jer su bili u names pa im je R zamijenio s .), no grad zagreb i RH ne trebaju -
  pom[,1][2:7] <- gsub(" ", "-",pom[,1][2:7])

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

write.csv(st_saldo, "./data/processed data/stanovništvo saldo migracije s inozemstvom.csv")
write.csv(st_doseljeni, "./data/processed data/stanovništvo broj doseljenih.csv")
write.csv(st_doseljeni_druga_zup, "./data/processed data/stanovništvo broj doseljenih iz druge županije.csv")
write.csv(st_doseljeni_druga_drz, "./data/processed data/stanovništvo broj broj doseljenih iz druge države.csv")
write.csv(st_odseljeni, "./data/processed data/stanovništvo broj odseljenih.csv")
write.csv(st_odseljeni_druga_zup, "./data/processed data/stanovništvo broj odseljenih u drugu županiju.csv")
write.csv(st_odseljeni_druga_drz, "./data/processed data/stanovništvo broj odseljenih u drugu državu.csv")

##7.2.3. PROCJENA UKUPNOG BROJA STANOVNIKA SREDINOM GODINE
stanovnistvo_procjena <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Stanovništvo.xlsx", sheetName = "7.2.3.",
                                startRow=7, encoding = "UTF-8", endRow=29,
                                colIndex = seq(1,17),
                                stringsAsFactors=F),17)

names(stanovnistvo_procjena)[3:17] <- gsub('.{2}$', '', names(stanovnistvo_procjena)[3:17] ) #micanje anotacija s godina
write.csv(stanovnistvo_procjena, "./data/processed data/stanovništvo procjena br st sredinom godine.csv")


# Transport.xlsx #############################
tran_cestovna <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.1.",
                                   startRow=9, encoding = "UTF-8", endRow=32,
                                   colIndex = seq(1,13),
                                   stringsAsFactors=F), 13)
tran_cestovna <- tran_cestovna[-c(2),]
write.csv(tran_cestovna, "./data/processed data/transport ukupni broj cestovnih mreža.csv")

#4
tran_sheet1 <-c("autocesta.csv", "državnih cesta.csv", "županijskih cesta.csv", "lokalnih cesta.csv")
tran_stupci <- c(15,27,39,51)
for(i in seq(1,4)){
  tran_ceste <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.1.",
                             startRow=9, encoding = "UTF-8", endRow=32,
                             colIndex = seq(tran_stupci[i],tran_stupci[i]+10), 
                             stringsAsFactors=F)
  tran_ceste <- tran_ceste[-c(2),]
  ##autoceste nemaju vrijednost u prva dva stupca, a R odbija prepoznati "..."
  if(i == 1){
    tran_ceste[1:2] <-NA
  }
  tran_ceste <- ocisti_dataframe(cbind(tran_cestovna[,1:2], tran_ceste), 13)
  write.csv(tran_ceste, paste("./data/processed data/transport ukupni broj", tran_sheet1[i]))
}
rm(tran_ceste)

tran_gustoca <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.2.",
                                            startRow=8, encoding = "UTF-8", endRow=31,
                                            colIndex = seq(1,13),
                                            stringsAsFactors=F)
tran_gustoca <- ocisti_dataframe(tran_gustoca[-c(2),], 13)
write.csv(tran_gustoca, "./data/processed data/transport gustoća cestovne mreže.csv")

tran_prijevoz_robe <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = "5.4.3.",
                                           startRow=9, encoding = "UTF-8", endRow=32,
                                           colIndex = seq(1,17),
                                           stringsAsFactors=F)
tran_prijevoz_robe <- ocisti_dataframe(tran_prijevoz_robe[-c(2),], 17)
write.csv(tran_prijevoz_robe, "./data/processed data/transport cestovni prijevoz robe.csv")

tran_nesrece_imena <- c("ukupno.csv", "s poginulim osobama.csv", "s ozlijeđenim osobama.csv")
tran_sheetovi <- c("5.4.4.","5.4.5.")

for (j in c(1,2)){
  for(i in seq(1,3)){
    tran_nesrece <- read.xlsx("./data/statistika u nizu/Transport.xlsx", sheetName = tran_sheetovi[j],
                                                     startRow=7, encoding = "UTF-8", endRow=29,
                                                     colIndex = seq( ifelse(i==1,1, ifelse(i==2,17,31)), ifelse(i==1,15, ifelse(i==2,29,43))),
                                                     stringsAsFactors=F)
    tran_nesrece <- tran_nesrece[-c(2),]
    if(i!= 1){
      tran_nesrece <- cbind(tran_zup, tran_nesrece)
    }else{
      tran_zup <- tran_nesrece[,1:2]
    }
    tran_nesrece <- ocisti_dataframe(tran_nesrece,14)
    write.csv(tran_nesrece, paste("./data/processed data/transport broj",ifelse(j==1, "prometnih nesreća", "nastradalih u prometu"), tran_nesrece_imena[i]))
  }
}
rm(tran_zup)
rm(tran_nesrece)

##TODO: 5.4.6. i 5.4.7. zračne luke, 5.4.8. registrirana vozila i 5.4.9. željeznički promet






# Turizam.xlsx #############################
#4.3.2.1. DOLASCI TURISTA U KOMERCIJALNIM SMJEŠTAJNIM OBJEKTIMA
tur_sheet <- c("4.3.2.1.", "4.3.2.2.")
tur_imena <- c("ukupno.csv", "domaći.csv", "strani.csv")
for(j in c(1,2)){
  for(i in seq(1,3)){
    if(j == 2 && i == 3) break()
    turizam_dolasci <- read.xlsx("./data/statistika u nizu/Turizam.xlsx", sheetName = tur_sheet[j],
                                                     startRow=7, encoding = "UTF-8", endRow=30,
                                                     colIndex = seq(ifelse(i==1, 1, ifelse(i==2, 26, 49)),
                                                                    ifelse(i==1, 24, ifelse(i==2, 47, 70))),
                                                     stringsAsFactors=F)
    turizam_dolasci <- turizam_dolasci[-c(2),]
    #Anotacije na 14. i 16. retku od naziva županija
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
    turizam_dolasci <- ocisti_dataframe(turizam_dolasci, 24)
    write.csv(turizam_dolasci, paste("./data/processed data/turizam broj",ifelse(j==1, "dolazaka", "noćenja"), ifelse(i == 2 && j==2, tur_imena[i+1], tur_imena[i])))
  }
}
rm(tur_nam)
rm(tur_zup)
rm(turizam_dolasci)

#4.3.2.3. LUKE NAUTIČKOG TURIZMA
tur_imena <- c("luka u nautičkom turizmu.csv", "plovila na stalnom vezu u lukama.csv", "plovila u tranzitu u lukama.csv")
for(i in seq(1,3)){
  tur_luke <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Turizam.xlsx", sheetName = "4.3.2.3.",
                            startRow=7, encoding = "UTF-8", endRow=14,
                            colIndex = seq( ifelse(i==1,1, ifelse(i==2,15,27)), ifelse(i==1,13, ifelse(i==2,25,37))),
                            stringsAsFactors=F),ifelse(i==1,13,11))
  if(i!= 1){
    tur_luke <- cbind(tur_zup, tur_luke)
  }else{
    tur_zup <- tur_luke[,1:2]
  }
  write.csv(tur_luke, paste("./data/processed data/turizam broj", tur_imena[i]))
}
rm(tur_zup)
rm(tur_luke)

# Zaposlenost i place.xlsx #############################
#faith in humanity restored!
zaposlenost <- function(sheet, br_podataka, imena, pocetni_red, zadnji_stupac){
  zap1 <- ocisti_dataframe(read.xlsx("./data/statistika u nizu/Zaposlenost i place.xlsx", sheetName = sheet,
                                    startRow=pocetni_red, encoding = "UTF-8", endRow=pocetni_red+22,
                                    colIndex = seq(1,zadnji_stupac), stringsAsFactors=F),zadnji_stupac)
  write.csv(zap1, paste("./data/processed data/zaposlenost", imena[1], ".csv"))
  if(br_podataka > 1){
    zap2 <- read.xlsx("./data/statistika u nizu/Zaposlenost i place.xlsx", sheetName = sheet,
                                       startRow=7, encoding = "UTF-8", endRow=29,
                                       colIndex = seq(25,45), stringsAsFactors=F)
    zap2 <- ocisti_dataframe(cbind(zap1[,1:2], zap2),23)
    names(zap2) <- names(zap1)
    write.csv(zap2, paste("./data/processed data/zaposlenost", imena[2], ".csv"))
  }
}

zaposlenost("9.2.1.", 2, c("neto plaće", "bruto plaće"), 7, 23)
zaposlenost("9.2.2.", 1, c("stopa registrirane nezaposlenosti"),8, 19)
zaposlenost("9.2.3.", 1, c("nezaposleni"),7,19)
zaposlenost("9.2.4.", 1, c("aktivni osiguranici-individualni poljoprivrednici"),7,19)
zaposlenost("9.2.5.", 1, c("zaposleni u obrtu i slobodnim profesijama"),7,19)
zaposlenost("9.2.6.", 1, c("zaposleni u pravnim osobama"),7,19)
zaposlenost("9.2.7.", 1, c("aktivno stanovništvo"),7,19)
zaposlenost("9.2.8.", 1, c("ukupno zaposleni"),7,19)