library(tidyverse)

#pakraunam duomenis
dedal <- read.csv("data/Dedal.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
eurolinen <- read.csv("data/EUROLINEN.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
technocolor <- read.csv("data/technocolor.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
translogistika <- read.csv("data/Translogistika.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
grieshaber <- read.csv("data/Grieshaber.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
gw <- read.csv("data/GW.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
transuniverse <- read.csv("data/Transuniverse.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
nuova <- read.csv("data/NUOVA TRANSPORT.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
edp <- read.csv("data/EDP.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
btk <- read.csv("data/BTK.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
ebrotrans <- read.csv("data/ebrotrans.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
lkw_walter_kufstein <- read.csv("data/LKW Walter A-Kufstein.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
valsped <- read.csv("data/Valsped.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
fg_turin <- read.csv("data/FG Turin.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
fg_dalmine <- read.csv("data/FG Dalmine.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
ispanija <- read.csv("data/Ispanija.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
dsv_road_ooo <- read.csv("data/DSV ROAD OOO.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")

#nurodom koks skyrius kurio dataframe
dedal$klientas <- "Dedal"
eurolinen$klientas <- "Eurolinen"
technocolor$klientas <- "Technocolor"
translogistika$klientas <- "Translogistika"
grieshaber$klientas <- "Grieshaber"
gw$klientas <- "GW"
transuniverse$klientas <- "Transuniverse"
nuova$klientas <- "Nuova"
edp$klientas <- "EDP"
btk$klientas <- "BTK"
ebrotrans$klientas <- "Ebrotrans"
lkw_walter_kufstein$klientas <- "LKW Walter Kufstein"
valsped$klientas <- "Valsped"
fg_turin$klientas <- "FG Turin"
fg_dalmine$klientas <- "FG Dalmine"
ispanija$klientas <- "Ispanija"
dsv_road_ooo$klientas <- "DSV Road OOO"

#surišam į vieną
data <- bind_rows(mutate_all(dedal,as.character),mutate_all(eurolinen,as.character),mutate_all(technocolor,as.character),
                  mutate_all(translogistika,as.character),mutate_all(grieshaber,as.character),mutate_all(gw,as.character),
                  mutate_all(transuniverse,as.character),mutate_all(nuova,as.character),mutate_all(edp,as.character),
                  mutate_all(btk,as.character),mutate_all(ebrotrans,as.character),mutate_all(lkw_walter_kufstein,as.character),
                  mutate_all(valsped,as.character),mutate_all(fg_turin,as.character),mutate_all(fg_dalmine,as.character),
                  mutate_all(ispanija,as.character),mutate_all(dsv_road_ooo,as.character))

#randam kurį mėnesį baigėsi reisas
data$menuo <- substr(data$atvykimo_data,4,5)

#randam kuris ketvirtis
data[data$menuo %in% c("01","02","03"),"ketvirtis"] <- "01"
data[data$menuo %in% c("04","05","06"),"ketvirtis"] <- "02"
data[data$menuo %in% c("07","08","09"),"ketvirtis"] <- "03"
data[data$menuo %in% c("10","11","12"),"ketvirtis"] <- "04"

#skaičiuojam frachtus paketvirčiui kiekvienam klientui
data$frachtas <- as.numeric(data$frachtas)
frachtai_klientas_ketvirtis <- aggregate(frachtas ~ klientas + ketvirtis, data = data, FUN = sum)

#skaičiuojam pelną paketvirčiui kiekvienam klientui
data$pelnas_pries_pap_islaidas <- as.numeric(data$pelnas_pries_pap_islaidas)
pelnas_klientas_ketvirtis <- aggregate(pelnas_pries_pap_islaidas ~ klientas + ketvirtis, data = data, FUN = sum)
pelnas_proc_klientas_ketvirtis <- left_join(frachtai_klientas_ketvirtis,pelnas_klientas_ketvirtis)
pelnas_proc_klientas_ketvirtis$pelnas_proc <- pelnas_proc_klientas_ketvirtis$pelnas_pries_pap_islaidas / 
  pelnas_proc_klientas_ketvirtis$frachtas * 100

# jei pelnas neigiamas, statom 0
pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$pelnas_proc < 0,"pelnas_proc"] <- 0

#paskutinio ketvirčio pelno grafikas
pelnas_plot <- pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$ketvirtis == "04",]
ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_proc), y = pelnas_proc)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_proc), by = 5),0)) +
  ylab("Pelnas prieš pastovias išlaidas (%) 4 ketvirtis 2018") + 
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0))

