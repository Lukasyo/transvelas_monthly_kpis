# skriptas atlieka šiuos pakeitimus:
# 1. pakeičia praėjusio mėnesio keliu mokesčius į naujai išprognozuotus (pvz.: jegu šiandien sausis, gruo-
# džio mėnuo bus pakeistas prognozuojamais, net jei duomenys yra; žr. trūkstamų rodiklių prognozavimas
# sekciją
# 2. Valsped ir Nuova frachtai praėjusiam mėnesiui yra prognozuojami.

library(tidyverse)
library(lubridate)

# Duomenų paruošimas ------------------------------------------------------

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

# pašalinti reisus, kur trūksta duomenų, pvz. kuro
data <- data[!data$kuras_eur_km == "0",]
data <- data[!data$vair_atlyg_dienai == "0",]

# Kelių mokesčio prognozė --------------------------------------------

# prognozuojam praėjusio mėnesio kelių mokesčius imam prieš tai buvusių 4 mėnesių vidurkį pagal klientą

# nustatomi 4 menesiai, buvę iki praėjusio mėnesio, pvz.: jeigu šiandien sausis, tai praėjęs mėnuo bus
# gruodis (kuriam mes dar neturime kelių mokesčio), o 4 mėnesiai, naudojami skaičiavimui bus rugpjūtis, 
# rugsėjis, spalis ir lapkritis.

menesiai <- c(month(Sys.Date() %m+% months(-2)),month(Sys.Date() %m+% months(-3)),
                                                     month(Sys.Date() %m+% months(-4)),
                                                           month(Sys.Date() %m+% months(-5)))

menuo <- as.character(month(Sys.Date() %m+% months(-1)))
data$keliai_eur_km <- as.numeric(data$keliai_eur_km)
kdata <- data[data$menuo %in% menesiai,]
keliai_pagal_klienta <- aggregate(keliai_eur_km ~ klientas, data = kdata, FUN = mean)
keliai_pagal_klienta$menuo <- menuo

# keičiam praėjusio mėnesio kelių mokesčius į naujai apskaičiuotus

for (k in 1:nrow(keliai_pagal_klienta)) {
  
  data[data$klientas == keliai_pagal_klienta[k,"klientas"] & 
         data$menuo == menuo,"keliai_eur_km"] <- keliai_pagal_klienta[k,"keliai_eur_km"]
  
}


# Valsped ir Nuova frachtų prognozė ---------------------------------------
# kadangi kai kurių klientų sąskaitos ateina vėlai, reikia nuprognozuoti. (Tokiu pačiu principu kaip ir 
# kelius)

data$frachtas_km <- as.numeric(data$frachtas_km)
vdata <- data[data$klientas %in% c("Valsped","Nuova", "EDP") & data$menuo %in% menesiai,]
frachtu_prognoze <- aggregate(frachtas_km ~ klientas, data = vdata, FUN = mean)

# keičiam praėjusio mėnesio Valsped ir Nuova frachtus_km į naujus išprognozuotus

for (k in 1:nrow(frachtu_prognoze)) {
  
  data[data$klientas == frachtu_prognoze[k,"klientas"] & 
         data$menuo == menuo,"frachtas_km"] <- frachtu_prognoze[k,"frachtas_km"]
  
}

# taip pat atnaujinam ir frachtas ir frachtas dienai rodiklius pagal naujai išskaičiuotą frachtas km

data$is_viso_km <- as.numeric(data$is_viso_km)
data[data$klientas %in% c("Nuova","Valsped","EDP"),
     "frachtas"] <- data[data$klientas %in% c("Nuova","Valsped","EDP"),"frachtas_km"] * 
     data[data$klientas %in% c("Nuova","Valsped","EDP"),"is_viso_km"]

data[data$klientas %in% c("Nuova","Valsped","EDP"),
     "frachtas_dienai"] <- data[data$klientas %in% c("Nuova","Valsped","EDP"),"frachtas"] / 
  data[data$klientas %in% c("Nuova","Valsped","EDP"),"is_viso_dienu"]

# atnaujinam ir kitus išvestinius rodiklius
data$pelnas_pries_pap_islaidas <- as.numeric(data$pelnas_pries_pap_islaidas)
data$reiso_islaidos <- as.numeric(data$reiso_islaidos)
data[data$klientas %in% c("Nuova","Valsped","EDP"),
     "pelnas_pries_pap_islaidas"] <- data[data$klientas %in% c("Nuova","Valsped","EDP"),"frachtas"] - 
  data[data$klientas %in% c("Nuova","Valsped","EDP"),"reiso_islaidos"]

data[data$klientas %in% c("Nuova","Valsped","EDP"),
     "pelnas_dienai"] <- data[data$klientas %in% c("Nuova","Valsped","EDP"),"pelnas_pries_pap_islaidas"] / 
  data[data$klientas %in% c("Nuova","Valsped","EDP"),"is_viso_dienu"]

data[data$klientas %in% c("Nuova","Valsped","EDP"),
     "pelnas_km"] <- data[data$klientas %in% c("Nuova","Valsped","EDP"),"pelnas_pries_pap_islaidas"] / 
  data[data$klientas %in% c("Nuova","Valsped","EDP"),"is_viso_km"]

# Klientų rodiklių skaičiavimas -------------------------------------------

#skaičiuojam frachtus dienai paketvirčiui kiekvienam klientui
data$frachtas <- as.numeric(data$frachtas)
data$is_viso_dienu <- as.integer(data$is_viso_dienu)
data$frachtas_dienai <- data$frachtas/data$is_viso_dienu
frachtai_klientas_ketvirtis <- aggregate(frachtas_dienai ~ klientas + ketvirtis, data = data, FUN = sum)

#skaičiuojam pelną dienai paketvirčiui kiekvienam klientui
data$pelnas_dienai <- as.numeric(data$pelnas_dienai)
pelnas_klientas_ketvirtis <- aggregate(pelnas_dienai ~ klientas + ketvirtis, data = data, FUN = sum)
pelnas_proc_klientas_ketvirtis <- left_join(frachtai_klientas_ketvirtis,pelnas_klientas_ketvirtis)
pelnas_proc_klientas_ketvirtis$pelnas_proc <- pelnas_proc_klientas_ketvirtis$pelnas_dienai / 
  pelnas_proc_klientas_ketvirtis$frachtas_dienai * 100

# jei pelnas neigiamas, statom 0
pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$pelnas_proc < 0,"pelnas_proc"] <- 0


# kuras, keliai, vair. atlyginimas, frachtas, pelnas kilometrui vienam grafike bendras --------
data$kuras_eur_km <- as.numeric(data$kuras_eur_km)
data$islaidos_kurui <- data$kuras_eur_km * data$is_viso_km
plot_data <- aggregate(islaidos_kurui ~ menuo, data = data, FUN = sum)
kilometrai <- aggregate(is_viso_km ~ menuo, data = data, FUN= sum)
plot_data <- left_join(plot_data,kilometrai)
plot_data$kuras_eur_km <- plot_data$islaidos_kurui / plot_data$is_viso_km
data$islaidos_keliams <- data$keliai_eur_km * data$is_viso_km
keliai <- aggregate(islaidos_keliams ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,keliai)
plot_data$keliai_eur_km <- plot_data$islaidos_keliams / plot_data$is_viso_km
data$vair_atlyg_km <- as.numeric(data$vair_atlyg_1_km)
data$islaidos_vairams <- data$vair_atlyg_km * data$is_viso_km
vairai <- aggregate(islaidos_vairams ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,vairai)
plot_data$vair_atlyg_km <- plot_data$islaidos_vairams / plot_data$is_viso_km
frachtai <- aggregate(frachtas ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,frachtai)
plot_data$frachtas_km <- plot_data$frachtas / plot_data$is_viso_km
pelnai <- aggregate(pelnas_pries_pap_islaidas ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,pelnai)
plot_data$pelnas_km <- plot_data$pelnas_pries_pap_islaidas / plot_data$is_viso_km

plot_data2 <- plot_data[,c("kuras_eur_km","keliai_eur_km","vair_atlyg_km","frachtas_km","pelnas_km","menuo")]
plot_data_melt2 <- melt(plot_data2, id = "menuo")

ggplot(data = plot_data_melt2, aes(x = menuo, y = value, group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(breaks = round(seq(0, max(plot_data_melt2$value), by = 0.05),2)) +
  ylab("EUR") +
  xlab("Atvykimo mėnuo") +
  ggtitle("Frachtas, išlaidos ir pelnas kilometrui") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24))






# Pelnas dienai % pagal klientus, 4 ketvirtis vs metai --------------------

#paskutinio ketvirčio pelno grafikas
pelnas_plot <- pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$ketvirtis == "04",]
ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_proc), y = pelnas_proc)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_proc), by = 5),0)) +
  ylab("Pelnas dienai prieš pastovias išlaidas (%) 4 ketvirtis 2018") + 
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0))

