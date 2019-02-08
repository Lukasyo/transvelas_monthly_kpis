# skriptas atlieka šiuos pakeitimus:
# 1. pakeičia praėjusio mėnesio keliu mokesčius į naujai išprognozuotus (pvz.: jegu šiandien sausis, gruo-
# džio mėnuo bus pakeistas prognozuojamais, net jei duomenys yra; žr. trūkstamų rodiklių prognozavimas
# sekciją
# 2. Valsped, EDP ir Nuova frachtai praėjusiam mėnesiui yra prognozuojami.
# 3. Atima Nuova ir Eurolinen komisinius iš frachtų prieš analizę

# mintis ateičiai - frachtus skaičiuoti atėmus kelių mokesčius, nes skirtingose šalyse jie skirtingi.

library(tidyverse)
library(lubridate)
library(reshape2)
library(openxlsx)

# ataskaitinis ketvirtis
ketvirtis <- "04"
# ketvirtis palyginimui
lyg_ketvirtis <- "03"


#komisiniai

komisiniai_klientai <- c("Nuova","Eurolinen")

nuova_komisinis <- 0.092 #t.y. 9.2%

# eurolinen
by <- 29 #eur
be_fr <- 100 #eur
it <- 145 #eur

# Duomenų paruošimas ------------------------------------------------------

#pakraunam duomenis
all <- read.csv("data/ALL.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
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
reisas_masina <- read.csv("data/masina_kelialapis.csv", stringsAsFactors=FALSE, encoding = "UTF-8", dec = ",")
masina_modelis <- read.xlsx("data/numeris_modelis.xlsx")

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

# pašalint nulius iš mėnesių
data$menuo <- as.integer(gsub("0","",data$menuo))
data[data$menuo == "1" & data$ketvirtis == "04","menuo"] <- "10"
data$menuo <- as.integer(data$menuo)

# pridedam mašiną prie duomenų
data <- left_join(data, reisas_masina)

# ištrinam nereikalingus kintamuosius
drops <- c("pelnas_po_kuro_dienai","grupe","pretenzija_visa_suma","frachtas_be_keltu",
           "pelnas_po_pap_islaidu","pap_kint_islaidos","pap_past_islaidos","pelnas_po_pap_isl_dienai",
           "islosta_ant_kuro","pretenzija_susigrazinta_is_vair", "pretenzija_islaidos_reisui",
           "keltu_islaidos","islaidos_be_keltu")
data <- data[ , !(names(data) %in% drops)]

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


# Valsped, EDP ir Nuova frachtų prognozė ---------------------------------------
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
data[data$klientas %in% c("Valsped","Nuova","EDP"),"frachtas"] <- data[data$klientas %in% c("Valsped","Nuova","EDP"),"frachtas_km"] * 
     data[data$klientas %in% c("Valsped","Nuova","EDP"),"is_viso_km"]

data$frachtas <- as.numeric(data$frachtas)
data$is_viso_dienu <- as.numeric(data$is_viso_dienu)

data[data$klientas %in% c("Valsped","Nuova","EDP"),"frachtas_dienai"] <- data[data$klientas %in% c("Valsped","Nuova","EDP"),"frachtas"] / 
  data[data$klientas %in% c("Valsped","Nuova","EDP"),"is_viso_dienu"]

# atnaujinam ir kitus išvestinius rodiklius
data$pelnas_pries_pap_islaidas <- as.numeric(data$pelnas_pries_pap_islaidas)
data$reiso_islaidos <- as.numeric(data$reiso_islaidos)
data[data$klientas %in% c("Valsped","Nuova","EDP"),"pelnas_pries_pap_islaidas"] <- data[data$klientas %in% c("Valsped","Nuova","EDP"),"frachtas"] - 
  data[data$klientas %in% c("Valsped","Nuova","EDP"),"reiso_islaidos"]

data[data$klientas %in% c("Valsped","Nuova","EDP"),"pelnas_dienai"] <- data[data$klientas %in% c("Valsped","Nuova","EDP"),"pelnas_pries_pap_islaidas"] / 
  data[data$klientas %in% c("Valsped","Nuova","EDP"),"is_viso_dienu"]

data[data$klientas %in% c("Valsped","Nuova","EDP"),"pelnas_km"] <- data[data$klientas %in% c("Valsped","Nuova","EDP"),"pelnas_pries_pap_islaidas"] / 
  data[data$klientas %in% c("Valsped","Nuova","EDP"),"is_viso_km"]

# kuras, keliai, vair. atlyginimas, pelnas kilometrui vienam grafike bendras --------
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

# minusuojam klientų komisinius -------------------

#randam kuris reisas kur važiavo
reisas_masina[grepl("BY",reisas_masina$uzduotys),"kryptis"] <- "BY"
reisas_masina[grepl("BE",reisas_masina$uzduotys),"kryptis"] <- "BE_FR"
reisas_masina[grepl("FR",reisas_masina$uzduotys),"kryptis"] <- "BE_FR"
reisas_masina[grepl("IT",reisas_masina$uzduotys),"kryptis"] <- "IT"
data <- left_join(data,reisas_masina[,c("reisas","kryptis")])

# atimam Eurolinen komisinį iš frachtų
data[data$klientas == "Eurolinen" & data$kryptis == "BY", "frachtas"] <- as.numeric(data[data$klientas == "Eurolinen" &
                                                                                           data$kryptis == "BY", "frachtas"]) - by
data[data$klientas == "Eurolinen" & data$kryptis == "BE_FR", "frachtas"] <- as.numeric(data[data$klientas == "Eurolinen" &
                                                                                              data$kryptis == "BE_FR", "frachtas"]) - be_fr
data[data$klientas == "Eurolinen" & data$kryptis == "IT", "frachtas"] <- as.numeric(data[data$klientas == "Eurolinen" &
                                                                                           data$kryptis == "IT", "frachtas"]) - it

data[data$klientas == "Nuova","frachtas"] <- as.numeric(data[data$klientas == "Nuova","frachtas"]) * (1 - nuova_komisinis)

data[data$klientas %in% komisiniai_klientai,"frachtas_km"] <- as.numeric(data[data$klientas %in% komisiniai_klientai,"frachtas"]) /
  as.numeric(data[data$klientas %in% komisiniai_klientai,"is_viso_km"])
data[,"pelnas_pries_pap_islaidas"] <- as.numeric(data[,"frachtas"]) -
  as.numeric(data[,"reiso_islaidos"])
data[data$klientas %in% komisiniai_klientai,"pelnas_km"] <- as.numeric(data[data$klientas %in% komisiniai_klientai,"pelnas_pries_pap_islaidas"]) /
  as.numeric(data[data$klientas %in% komisiniai_klientai,"is_viso_km"])
data[data$klientas %in% komisiniai_klientai,"pelnas_dienai"] <- as.numeric(data[data$klientas %in% komisiniai_klientai,"pelnas_pries_pap_islaidas"]) /
  as.numeric(data[data$klientas %in% komisiniai_klientai,"is_viso_dienu"])

# Jungiam EDP/Nuova į vieną -----------------------------------------------

data[data$klientas %in% c("Nuova","EDP"), "klientas"] <- "EDP/Nuova"

# tęsinys kuras, keliai, vair. atlyginimas, pelnas kilometrui vienam grafike bendras --------
frachtai <- aggregate(frachtas ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,frachtai)
plot_data$frachtas_km <- plot_data$frachtas / plot_data$is_viso_km
pelnai <- aggregate(pelnas_pries_pap_islaidas ~ menuo, data = data, FUN = sum)
plot_data <- left_join(plot_data,pelnai)
plot_data$pelnas_km <- plot_data$pelnas_pries_pap_islaidas / plot_data$is_viso_km

plot_data2 <- plot_data[,c("kuras_eur_km","keliai_eur_km","vair_atlyg_km","pelnas_km","menuo")]
plot_data_melt2 <- melt(plot_data2, id = "menuo")

ggplot(data = plot_data_melt2, aes(x = menuo, y = value, group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(breaks = round(seq(0, max(plot_data_melt2$value), by = 0.05),2)) +
  scale_x_continuous(breaks = round(seq(min(plot_data_melt2$menuo),max(plot_data_melt2$menuo), by = 1),0)) +
  ylab("EUR") +
  xlab("Atvykimo mėnuo") +
  ggtitle("Išlaidos ir pelnas kilometrui") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# Klientų rodiklių skaičiavimas -------------------------------------------

#skaičiuojam frachtus dienai paketvirčiui kiekvienam klientui
data$frachtas <- as.numeric(data$frachtas)
data$is_viso_dienu <- as.integer(data$is_viso_dienu)
data$frachtas_dienai <- data$frachtas/data$is_viso_dienu
frachtai_klientas_ketvirtis <- aggregate(frachtas_dienai ~ klientas + ketvirtis, data = data, FUN = sum)

#skaičiuojam pelną dienai paketvirčiui kiekvienam klientui
pelnas_klientas_ketvirtis <- aggregate(pelnas_pries_pap_islaidas ~ klientas + ketvirtis, 
                                       data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = sum)
pelnas_klientas_ketvirtis <- left_join(pelnas_klientas_ketvirtis,reisai_klientas)
pelnas_klientas_ketvirtis$pelnas_dienai2 <- pelnas_klientas_ketvirtis$pelnas_pries_pap_islaidas / 
  pelnas_klientas_ketvirtis$is_viso_dienu
pelnas_proc_klientas_ketvirtis <- left_join(frachtai_klientas_ketvirtis,pelnas_klientas_ketvirtis)
pelnas_proc_klientas_ketvirtis$pelnas_proc <- pelnas_proc_klientas_ketvirtis$pelnas_dienai2 / 
  pelnas_proc_klientas_ketvirtis$frachtas_dienai * 100

# skaičiuojam metinį vidutinį pelną dienai kiekvienam klientui
pelnas_klientas_metai <- aggregate(pelnas_pries_pap_islaidas  ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = sum)
pelnas_klientas_metai <- left_join(pelnas_klientas_metai,reisai_klientas)
pelnas_klientas_metai$pelnas_dienai2 <- pelnas_klientas_metai$pelnas_pries_pap_islaidas /
  pelnas_klientas_metai$is_viso_dienu

# jei pelnas neigiamas, statom 0
pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$pelnas_proc < 0,"pelnas_proc"] <- 0

# Pokytis praėjusio ketvirčio lyginant su ankstesniu -----------------

# kuras, keliai, vair. atlyginimas, pelnas kilometrui ketvirčiui ! --------
data$kuras_eur_km <- as.numeric(data$kuras_eur_km)
data$islaidos_kurui <- data$kuras_eur_km * data$is_viso_km
plot_data <- aggregate(islaidos_kurui ~ ketvirtis, data = data, FUN = sum)
kilometrai <- aggregate(is_viso_km ~ ketvirtis, data = data, FUN= sum)
plot_data <- left_join(plot_data,kilometrai)
plot_data$kuras_eur_km <- plot_data$islaidos_kurui / plot_data$is_viso_km
data$islaidos_keliams <- data$keliai_eur_km * data$is_viso_km
keliai <- aggregate(islaidos_keliams ~ ketvirtis, data = data, FUN = sum)
plot_data <- left_join(plot_data,keliai)
plot_data$keliai_eur_km <- plot_data$islaidos_keliams / plot_data$is_viso_km
data$vair_atlyg_km <- as.numeric(data$vair_atlyg_1_km)
data$islaidos_vairams <- data$vair_atlyg_km * data$is_viso_km
vairai <- aggregate(islaidos_vairams ~ ketvirtis, data = data, FUN = sum)
plot_data <- left_join(plot_data,vairai)
plot_data$vair_atlyg_km <- plot_data$islaidos_vairams / plot_data$is_viso_km
frachtai <- aggregate(frachtas ~ ketvirtis, data = data, FUN = sum)
plot_data <- left_join(plot_data,frachtai)
plot_data$frachtas_km <- plot_data$frachtas / plot_data$is_viso_km
pelnai <- aggregate(pelnas_pries_pap_islaidas ~ ketvirtis, data = data, FUN = sum)
plot_data <- left_join(plot_data,pelnai)
plot_data$pelnas_km <- plot_data$pelnas_pries_pap_islaidas / plot_data$is_viso_km

# praėjusio ketvirčio rezultatai
praejes <- plot_data[plot_data$ketvirtis == ketvirtis,]
anks <- plot_data[plot_data$ketvirtis == lyg_ketvirtis,]
praejes <- praejes[,c("frachtas_km","kuras_eur_km","keliai_eur_km","vair_atlyg_km","pelnas_km")]
anks <- anks[,c("frachtas_km","kuras_eur_km","keliai_eur_km","vair_atlyg_km","pelnas_km")]
pokytis_proc <- (praejes/anks- 1) * 100
pokytis_ct <- (praejes-anks) * 100
pok <- rbind(pokytis_proc,pokytis_ct)

# Pelnas dienai % pagal klientus, 4 ketvirtis vs metai --------------------
# 
# #paskutinio ketvirčio pelno grafikas
# pelnas_plot <- pelnas_proc_klientas_ketvirtis[pelnas_proc_klientas_ketvirtis$ketvirtis == ketvirtis,]
# ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_proc), y = pelnas_proc)) +
#   geom_col(fill = "steelblue") +
#   scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_proc), by = 5),0)) +
#   ylab("%") + 
#   ggtitle("Pelnas dienai prieš pastovias išlaidas (%) 4 ketvirtis 2018") +
#   xlab("Klientas") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
#         axis.title.y = element_text(size = rel(1.4), angle = 90),
#         axis.title.x = element_text(size = rel(1.4), angle = 0),
#         plot.title = element_text(size = 24))


# Pelnas dienai EUR pagal klientus, 4 ketvirtis vs metai --------------------

#paskutinio ketvirčio pelno grafikas
pelnas_plot <- pelnas_klientas_ketvirtis[pelnas_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_dienai2), y = pelnas_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_dienai2), by = 10),0)) +
  ylab("EUR") + 
  ggtitle("Pelnas dienai prieš pastovias išlaidas (EUR) 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(pelnas_dienai2,0)), size = 5)

#metinis pelno grafikas
pelnas_plot <- pelnas_klientas_metai
ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_dienai2), y = pelnas_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_dienai2), by = 10),0)) +
  ylab("EUR") + 
  ggtitle("Pelnas dienai prieš pastovias išlaidas (EUR) 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(pelnas_dienai2,0)), size = 5)


# KM dienai pagal klientus, 4 ketvirtis vs metai --------------------------

#skaičiuojam KM dienai paketvirčiui kiekvienam klientui
data$km_dienai <- as.numeric(data$km_dienai)
km_klientas_ketvirtis <- aggregate(km_dienai ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
km_klientas_ketvirtis <- left_join(km_klientas_ketvirtis,reisai_klientas)
km_klientas_ketvirtis$km_dienai2 <- km_klientas_ketvirtis$km_dienai/km_klientas_ketvirtis$is_viso_dienu

# skaičiuojam metinį vidutinį KM dienai kiekvienam klientui
km_klientas_metai <- aggregate(km_dienai ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
km_klientas_metai <- left_join(km_klientas_metai,reisai_klientas)
km_klientas_metai$km_dienai2 <- km_klientas_metai$km_dienai/km_klientas_metai$is_viso_dienu


#paskutinio ketvirčio km dienai grafikas
km_plot <- km_klientas_ketvirtis[km_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = km_plot, aes(x = reorder(klientas, -km_dienai2), y = km_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(km_plot$km_dienai2), by = 50),0)) +
  ylab("KM") + 
  ggtitle("Kilometrai dienai 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(km_dienai2,0)), size = 5)

#metinis km grafikas
km_plot <- km_klientas_metai
ggplot(data = km_plot, aes(x = reorder(klientas, -km_dienai2), y = km_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(km_plot$km_dienai2), by = 50),0)) +
  ylab("KM") + 
  ggtitle("Kilometrai dienai 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(km_dienai2,0)), size = 5)

# Išlaidos kurui per kilometrą --------------------------------------------

#skaičiuojam išlaidas kurui per kilometrą paketvirčiui kiekvienam klientui
data$kuras_eur_km <- as.numeric(data$kuras_eur_km)
kuras_eur_km_klientas_ketvirtis <- aggregate(kuras_eur_km ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
kuras_eur_km_klientas_ketvirtis <- left_join(kuras_eur_km_klientas_ketvirtis,reisai_klientas)
kuras_eur_km_klientas_ketvirtis$kuras_eur_km2 <- kuras_eur_km_klientas_ketvirtis$kuras_eur_km/kuras_eur_km_klientas_ketvirtis$is_viso_dienu

# skaičiuojam metinį vidutines išlaidas kurui kilometrui kiekvienam klientui
kuras_eur_km_klientas_metai <- aggregate(kuras_eur_km ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
kuras_eur_km_klientas_metai <- left_join(kuras_eur_km_klientas_metai,reisai_klientas)
kuras_eur_km_klientas_metai$kuras_eur_km2 <- kuras_eur_km_klientas_metai$kuras_eur_km/kuras_eur_km_klientas_metai$is_viso_dienu


#paskutinio ketvirčio išlaidų kurui kilometrui grafikas
kuras_eur_km_plot <- kuras_eur_km_klientas_ketvirtis[kuras_eur_km_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = kuras_eur_km_plot, aes(x = reorder(klientas, -kuras_eur_km2), y = kuras_eur_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(kuras_eur_km_plot$kuras_eur_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Išlaidos kurui kilometrui 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(kuras_eur_km2,2)), size = 5)

#metinis kuro išlaidų kilometrui grafikas
kuras_eur_km_plot <- kuras_eur_km_klientas_metai
ggplot(data = kuras_eur_km_plot, aes(x = reorder(klientas, -kuras_eur_km2), y = kuras_eur_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(kuras_eur_km_plot$kuras_eur_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Kuro išlaidos kilometrui 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(kuras_eur_km2,2)), size = 5)

# Kuro sąnaudos 100 km  --------------------------------------------

#skaičiuojam kuro sąnaudas paketvirčiui kiekvienam klientui
data$lt_100km_faktas <- as.numeric(data$lt_100km_faktas)
kuras_litrais_klientas_ketvirtis <- aggregate(lt_100km_faktas ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
kuras_litrais_klientas_ketvirtis <- left_join(kuras_litrais_klientas_ketvirtis,reisai_klientas)
kuras_litrais_klientas_ketvirtis$lt_100km_faktas2 <- kuras_litrais_klientas_ketvirtis$lt_100km_faktas/kuras_litrais_klientas_ketvirtis$is_viso_dienu

# skaičiuojam kuro sąnaudas litrais kiekvienam klientui metams
kuras_litrais_klientas_metai <- aggregate(lt_100km_faktas ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
kuras_litrais_klientas_metai <- left_join(kuras_litrais_klientas_metai,reisai_klientas)
kuras_litrais_klientas_metai$lt_100km_faktas2 <- kuras_litrais_klientas_metai$lt_100km_faktas/kuras_eur_km_klientas_metai$is_viso_dienu


#paskutinio ketvirčio išlaidų kurui kilometrui grafikas
kuras_litrais_plot <- kuras_litrais_klientas_ketvirtis[kuras_litrais_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = kuras_litrais_plot, aes(x = reorder(klientas, -lt_100km_faktas2), y = lt_100km_faktas2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(kuras_litrais_plot$lt_100km_faktas2), by = 5),1)) +
  ylab("Litrai") + 
  ggtitle("Kuro sąnaudos 100km faktas 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(lt_100km_faktas2,1)), size = 5)

#metinis kuro išlaidų kilometrui grafikas
kuras_litrais_plot <- kuras_litrais_klientas_metai
ggplot(data = kuras_litrais_plot, aes(x = reorder(klientas, -lt_100km_faktas2), y = lt_100km_faktas2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(kuras_litrais_plot$lt_100km_faktas2), by = 5),1)) +
  ylab("Litrai") + 
  ggtitle("Kuro sąnaudos 100km faktas 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(lt_100km_faktas2,1)), size = 5)

# Frachtas kilometrui --------------------------------------------

#skaičiuojam frachtą kilometrui paketvirčiui kiekvienam klientui
data$frachtas_km <- as.numeric(data$frachtas_km)
frachtas_km_klientas_ketvirtis <- aggregate(frachtas_km ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
frachtas_km_klientas_ketvirtis <- left_join(frachtas_km_klientas_ketvirtis,reisai_klientas)
frachtas_km_klientas_ketvirtis$frachtas_km2 <- frachtas_km_klientas_ketvirtis$frachtas_km/frachtas_km_klientas_ketvirtis$is_viso_dienu

# skaičiuojam frachtą kilometrui kiekvienam klientui metams
frachtas_km_klientas_metai <- aggregate(frachtas_km ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
frachtas_km_klientas_metai <- left_join(frachtas_km_klientas_metai,reisai_klientas)
frachtas_km_klientas_metai$frachtas_km2 <- frachtas_km_klientas_metai$frachtas_km/frachtas_km_klientas_metai$is_viso_dienu

#paskutinio ketvirčio frachto kilometrui grafikas
frachtas_km_plot <- frachtas_km_klientas_ketvirtis[frachtas_km_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = frachtas_km_plot, aes(x = reorder(klientas, -frachtas_km2), y = frachtas_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(frachtas_km_plot$frachtas_km2), by = 0.1),2)) +
  ylab("EUR") + 
  ggtitle("Frachtas kilometrui 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(frachtas_km2,2)), size = 5)

#metinis frachto kilometrui grafikas
frachtas_km_plot <- frachtas_km_klientas_metai
ggplot(data = frachtas_km_plot, aes(x = reorder(klientas, -frachtas_km2), y = frachtas_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(frachtas_km_plot$frachtas_km2), by = 0.1),2)) +
  ylab("EUR") + 
  ggtitle("Frachtas kilometrui 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(frachtas_km2,2)), size = 5)

# Frachtas dienai pagal klientus, 4 ketvirtis vs metai --------------------------

#skaičiuojam frachtą dienai paketvirčiui kiekvienam klientui
data$frachtas_dienai <- as.numeric(data$frachtas_dienai)
frachtas_klientas_ketvirtis <- aggregate(frachtas_dienai ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
frachtas_klientas_ketvirtis <- left_join(frachtas_klientas_ketvirtis,reisai_klientas)
frachtas_klientas_ketvirtis$frachtas_dienai2 <- frachtas_klientas_ketvirtis$frachtas_dienai/frachtas_klientas_ketvirtis$is_viso_dienu

# skaičiuojam metinį vidutinį frachtas dienai kiekvienam klientui
frachtas_klientas_metai <- aggregate(frachtas_dienai ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
frachtas_klientas_metai <- left_join(frachtas_klientas_metai,reisai_klientas)
frachtas_klientas_metai$frachtas_dienai2 <- frachtas_klientas_metai$frachtas_dienai/frachtas_klientas_metai$is_viso_dienu


#paskutinio ketvirčio frachtas dienai grafikas
frachtas_plot <- frachtas_klientas_ketvirtis[frachtas_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = frachtas_plot, aes(x = reorder(klientas, -frachtas_dienai2), y = frachtas_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(frachtas_plot$frachtas_dienai2), by = 50),0)) +
  ylab("EUR") + 
  ggtitle("Frachtas dienai 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(frachtas_dienai2,0)), size = 5)

#metinis frachto dienai grafikas
frachtas_plot <- frachtas_klientas_metai
ggplot(data = frachtas_plot, aes(x = reorder(klientas, -frachtas_dienai2), y = frachtas_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(frachtas_plot$frachtas_dienai2), by = 50),0)) +
  ylab("EUR") + 
  ggtitle("Frachtas dienai 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(frachtas_dienai2,0)), size = 5)

# Vairuotojų atlyginimas dienai pagal klientus, 4 ketvirtis vs metai --------------------------

#skaičiuojam atlyginimą dienai paketvirčiui kiekvienam klientui
data$vair_atlyg_dienai <- as.numeric(data$vair_atlyg_dienai)
vair_atlyg_klientas_ketvirtis <- aggregate(vair_atlyg_dienai ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
vair_atlyg_klientas_ketvirtis <- left_join(vair_atlyg_klientas_ketvirtis,reisai_klientas)
vair_atlyg_klientas_ketvirtis$vair_atlyg_dienai2 <- vair_atlyg_klientas_ketvirtis$vair_atlyg_dienai/vair_atlyg_klientas_ketvirtis$is_viso_dienu

# skaičiuojam metinį vidutinį vair_atlyg dienai kiekvienam klientui
vair_atlyg_klientas_metai <- aggregate(vair_atlyg_dienai ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
vair_atlyg_klientas_metai <- left_join(vair_atlyg_klientas_metai,reisai_klientas)
vair_atlyg_klientas_metai$vair_atlyg_dienai2 <- vair_atlyg_klientas_metai$vair_atlyg_dienai/vair_atlyg_klientas_metai$is_viso_dienu


#paskutinio ketvirčio vair_atlyg dienai grafikas
vair_atlyg_plot <- vair_atlyg_klientas_ketvirtis[vair_atlyg_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = vair_atlyg_plot, aes(x = reorder(klientas, -vair_atlyg_dienai2), y = vair_atlyg_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(vair_atlyg_plot$vair_atlyg_dienai2), by = 5),0)) +
  ylab("EUR") + 
  ggtitle("Vairuotojo atlyginimas dienai 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(vair_atlyg_dienai2,0)), size = 5)

#metinis vair_atlyg dienai grafikas
vair_atlyg_plot <- vair_atlyg_klientas_metai
ggplot(data = vair_atlyg_plot, aes(x = reorder(klientas, -vair_atlyg_dienai2), y = vair_atlyg_dienai2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(vair_atlyg_plot$vair_atlyg_dienai2), by = 5),0)) +
  ylab("EUR") + 
  ggtitle("Vairuotojo atlyginimas dienai 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(vair_atlyg_dienai2,0)), size = 5)

# Kelių mokesčiai kilometrui --------------------------------------------

#skaičiuojam kelių mokesčius kilometrui paketvirčiui kiekvienam klientui
data$keliai_eur_km <- as.numeric(data$keliai_eur_km)
keliai_eur_km_klientas_ketvirtis <- aggregate(keliai_eur_km ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
keliai_eur_km_klientas_ketvirtis <- left_join(keliai_eur_km_klientas_ketvirtis,reisai_klientas)
keliai_eur_km_klientas_ketvirtis$keliai_eur_km2 <- keliai_eur_km_klientas_ketvirtis$keliai_eur_km/keliai_eur_km_klientas_ketvirtis$is_viso_dienu

# skaičiuojam kelių mokesčius kilometrui kiekvienam klientui metams
keliai_eur_km_klientas_metai <- aggregate(keliai_eur_km ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
keliai_eur_km_klientas_metai <- left_join(keliai_eur_km_klientas_metai,reisai_klientas)
keliai_eur_km_klientas_metai$keliai_eur_km2 <- keliai_eur_km_klientas_metai$keliai_eur_km/keliai_eur_km_klientas_metai$is_viso_dienu

#paskutinio ketvirčio kelių mokesčių kilometrui grafikas
keliai_eur_km_plot <- keliai_eur_km_klientas_ketvirtis[keliai_eur_km_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = keliai_eur_km_plot, aes(x = reorder(klientas, -keliai_eur_km2), y = keliai_eur_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(keliai_eur_km_plot$keliai_eur_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Kelių mokesčiai kilometrui 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(keliai_eur_km2,2)), size = 5)

#metinis kelių mokesčio kilometrui grafikas
keliai_eur_km_plot <- keliai_eur_km_klientas_metai
ggplot(data = keliai_eur_km_plot, aes(x = reorder(klientas, -keliai_eur_km2), y = keliai_eur_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(keliai_eur_km_plot$keliai_eur_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Kelių mokesčiai kilometrui 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(keliai_eur_km2,2)), size = 5)

# Pelnas kilometrui --------------------------------------------

#skaičiuojam pelną kilometrui paketvirčiui kiekvienam klientui
data$pelnas_km <- as.numeric(data$pelnas_km)
pelnas_km_klientas_ketvirtis <- aggregate(pelnas_km ~ klientas + ketvirtis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis, data = data, FUN = length)
pelnas_km_klientas_ketvirtis <- left_join(pelnas_km_klientas_ketvirtis,reisai_klientas)
pelnas_km_klientas_ketvirtis$pelnas_km2 <- pelnas_km_klientas_ketvirtis$pelnas_km/pelnas_km_klientas_ketvirtis$is_viso_dienu

# skaičiuojam pelną kilometrui kiekvienam klientui metams
pelnas_km_klientas_metai <- aggregate(pelnas_km ~ klientas, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas, data = data, FUN = length)
pelnas_km_klientas_metai <- left_join(pelnas_km_klientas_metai,reisai_klientas)
pelnas_km_klientas_metai$pelnas_km2 <- pelnas_km_klientas_metai$pelnas_km/pelnas_km_klientas_metai$is_viso_dienu

#paskutinio ketvirčio pelno kilometrui grafikas
pelnas_km_plot <- pelnas_km_klientas_ketvirtis[pelnas_km_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = pelnas_km_plot, aes(x = reorder(klientas, -pelnas_km2), y = pelnas_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_km_plot$pelnas_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Pelnas kilometrui 4 ketvirtis 2018") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(pelnas_km2,2)), size = 5)

#metinis pelnas kilometrui grafikas
pelnas_km_plot <- pelnas_km_klientas_metai
ggplot(data = pelnas_km_plot, aes(x = reorder(klientas, -pelnas_km2), y = pelnas_km2)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_km_plot$pelnas_km2), by = 0.05),2)) +
  ylab("EUR") + 
  ggtitle("Pelnas kilometrui 2018 metai") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(pelnas_km2,2)), size = 5)


# Frachto struktūra pagal klientą praėjęs ketvirtis-----------------------------------------

strukturai <- data[,c("is_viso_dienu","pelnas_pries_pap_islaidas","islaidos_vairams","islaidos_keliams",
                      "islaidos_kurui","klientas","ketvirtis")]

strukturai <- melt(strukturai, id=c("klientas","ketvirtis"))

strukturai_agg <- aggregate(value ~ variable + klientas + ketvirtis, data = strukturai, FUN = sum)

dienos <- strukturai_agg[strukturai_agg$variable == "is_viso_dienu",]
colnames(dienos)[4] <- "is_viso_dienu"

strukturai_agg <- strukturai_agg[strukturai_agg$variable != "is_viso_dienu",]


strukturai_agg <- left_join(strukturai_agg,dienos[,c("is_viso_dienu","klientas","ketvirtis")])

strukturai_agg$variable_dienai <- strukturai_agg$value / strukturai_agg$is_viso_dienu

strukturai_agg <- strukturai_agg[strukturai_agg$ketvirtis == ketvirtis,]

strukturai_agg <- strukturai_agg[ , !(names(strukturai_agg) == "ketvirtis")]

pelnas <- strukturai_agg[strukturai_agg$variable == "pelnas_pries_pap_islaidas",c("klientas","variable_dienai")]
colnames(pelnas)[2] <- "pelnas_dienai2"

strukturai_agg <- left_join(strukturai_agg, pelnas)

ggplot(strukturai_agg, aes(y = variable_dienai, x = reorder(klientas, -pelnas_dienai2))) + 
              scale_fill_brewer(palette = "Spectral") + 
  geom_col(aes(fill=variable), 
                   col="black", 
                   size=.1) +
  ylab("EUR") + 
  ggtitle("Frachto dienai struktūra 4 ketvirtis 2018 metai (išrikiuota pagal pelną)") +
  xlab("Klientas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24))

# Frachto struktūra pagal klientą metams-----------------------------------------

strukturai <- data[,c("is_viso_dienu","pelnas_pries_pap_islaidas","islaidos_vairams","islaidos_keliams",
                      "islaidos_kurui","klientas","ketvirtis")]

strukturai <- melt(strukturai, id="klientas")

strukturai$value <- as.integer(strukturai$value)

strukturai_agg <- aggregate(value ~ variable + klientas, data = strukturai, FUN = sum)

dienos <- strukturai_agg[strukturai_agg$variable == "is_viso_dienu",]
colnames(dienos)[3] <- "is_viso_dienu"

strukturai_agg <- strukturai_agg[!strukturai_agg$variable %in% c("is_viso_dienu","ketvirtis"),]


strukturai_agg <- left_join(strukturai_agg,dienos[,c("is_viso_dienu","klientas")])

strukturai_agg$variable_dienai <- strukturai_agg$value / strukturai_agg$is_viso_dienu

pelnas <- strukturai_agg[strukturai_agg$variable == "pelnas_pries_pap_islaidas",c("klientas","variable_dienai")]
colnames(pelnas)[2] <- "pelnas_dienai2"

strukturai_agg <- left_join(strukturai_agg, pelnas)

ggplot(strukturai_agg, aes(y = variable_dienai, x = reorder(klientas, -pelnas_dienai2))) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_col(aes(fill=variable), 
           col="black", 
           size=.1) +
  ylab("EUR") + 
  ggtitle("Frachto dienai struktūra 2018 metai (išrikiuota pagal pelną)") +
  xlab("Klientas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24))


# kuras, keliai, vair. atlyginimas, pelnas kilometrui kiekvienam klientui atskirai--------

plot_data <- aggregate(islaidos_kurui ~ ketvirtis + klientas, data = data, FUN = sum)
kilometrai <- aggregate(is_viso_km ~ ketvirtis + klientas, data = data, FUN= sum)
plot_data <- left_join(plot_data,kilometrai)
plot_data$kuras_eur_km <- plot_data$islaidos_kurui / plot_data$is_viso_km
keliai <- aggregate(islaidos_keliams ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,keliai)
plot_data$keliai_eur_km <- plot_data$islaidos_keliams / plot_data$is_viso_km
vairai <- aggregate(islaidos_vairams ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,vairai)
plot_data$vair_atlyg_km <- plot_data$islaidos_vairams / plot_data$is_viso_km
frachtai <- aggregate(frachtas ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,frachtai)
plot_data$frachtas_km <- plot_data$frachtas / plot_data$is_viso_km
pelnai <- aggregate(pelnas_pries_pap_islaidas ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,pelnai)
plot_data$pelnas_km <- plot_data$pelnas_pries_pap_islaidas / plot_data$is_viso_km
dienos <- aggregate(is_viso_dienu ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,dienos)
plot_data$pelnas_dienai <- plot_data$pelnas_pries_pap_islaidas / plot_data$is_viso_dienu
plot_data$islaidos_kurui_dienai <- plot_data$islaidos_kurui / plot_data$is_viso_dienu
plot_data$km_dienai <- plot_data$is_viso_km / plot_data$is_viso_dienu
plot_data$frachtas_dienai <- plot_data$frachtas / plot_data$is_viso_dienu
data$lt_100km_faktas <- as.numeric(data$lt_100km_faktas)
data$kuro_sanaudos_l <- data$lt_100km_faktas * data$is_viso_km / 100
kuras_l <- aggregate(kuro_sanaudos_l ~ ketvirtis + klientas, data = data, FUN = sum)
plot_data <- left_join(plot_data,kuras_l)
plot_data$kuras_l_100km <- plot_data$kuro_sanaudos_l / plot_data$is_viso_km * 100
plot_data$vair_atlyg_dienai <- plot_data$islaidos_vairams / plot_data$is_viso_dienu

plot_data2 <- plot_data[,c("kuras_eur_km","keliai_eur_km","vair_atlyg_km","pelnas_km","ketvirtis","klientas")]
plot_data_melt2 <- melt(plot_data2, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt2[plot_data_melt2$klientas=="Ebrotrans",], aes(x = ketvirtis, y = value, 
  group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(breaks = round(seq(0, max(plot_data_melt2$value), by = 0.05),2)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("EUR") +
  xlab("Ketvirtis") +
  ggtitle("Ebrotrans išlaidos ir pelnas kilometrui") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))


# pelnas, kuras ir vairuotojo atlyginimas dienai kiekvienam klientui --------------------------

plot_data3 <- plot_data[,c("islaidos_kurui_dienai","pelnas_dienai","vair_atlyg_dienai","ketvirtis","klientas")]
plot_data_melt3 <- melt(plot_data3, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt3[plot_data_melt3$klientas=="LKW Walter Kufstein",], aes(x = ketvirtis, y = value, 
                                                                     group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(0, max(plot_data_melt3[plot_data_melt3$klientas=="LKW Walter Kufstein","value"])), 
                     breaks = round(seq(0, max(plot_data_melt3$value), by = 5),0)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("EUR") +
  xlab("Ketvirtis") +
  ggtitle("LKW Walter Kufstein išlaidos kurui ir pelnas dienai") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# kilometrai dienai

plot_data3 <- plot_data[,c("km_dienai","ketvirtis","klientas")]
plot_data_melt3 <- melt(plot_data3, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt3[plot_data_melt3$klientas=="LKW Walter Kufstein",], aes(x = ketvirtis, y = value, 
                                                                     group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(min(plot_data_melt3[plot_data_melt3$klientas=="LKW Walter Kufstein","value"])*0.5,
                                max(plot_data_melt3[plot_data_melt3$klientas=="LKW Walter Kufstein","value"])), 
                     breaks = round(seq(0, max(plot_data_melt3$value), by = 10),0)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("Kilometrai") +
  xlab("Ketvirtis") +
  ggtitle("LKW Walter Kufstein kilometrai dienai") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# frachtas dienai

plot_data3 <- plot_data[,c("frachtas_dienai","ketvirtis","klientas")]
plot_data_melt3 <- melt(plot_data3, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt3[plot_data_melt3$klientas=="Ebrotrans",], aes(x = ketvirtis, y = value, 
                                                                     group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(min(plot_data_melt3[plot_data_melt3$klientas=="Ebrotrans","value"])*0.8,
                                max(plot_data_melt3[plot_data_melt3$klientas=="Ebrotrans","value"])), 
                     breaks = round(seq(0, max(plot_data_melt3$value), by = 5),0)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("Eur") +
  xlab("Ketvirtis") +
  ggtitle("Ebrotrans frachtas dienai") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# frachtas kilometrui

plot_data3 <- plot_data[,c("frachtas_km","ketvirtis","klientas")]
plot_data_melt3 <- melt(plot_data3, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova",], aes(x = ketvirtis, y = value, 
                                                                    group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(min(plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova","value"])*0.8,
                                max(plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova","value"])),
                     breaks = round(seq(0, max(plot_data_melt3$value), by = 0.02),2)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("Eur") +
  xlab("Ketvirtis") +
  ggtitle("EDP/Nuova frachtas kilometrui") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# kuro sąnaudos

plot_data3 <- plot_data[,c("kuras_l_100km","ketvirtis","klientas")]
plot_data_melt3 <- melt(plot_data3, id = c("ketvirtis","klientas"))

# reikia rankiniu būdu pakeisti klientus
ggplot(data = plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova",], aes(x = ketvirtis, y = value, 
                                                                    group = variable, color = variable)) +
  geom_line(size = 2) +
  scale_y_continuous(limits = c(min(plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova","value"])*0.8,
                                max(plot_data_melt3[plot_data_melt3$klientas=="EDP/Nuova","value"])), 
                     breaks = round(seq(0, max(plot_data_melt3$value), by = 1),0)) +
  #scale_x_continuous(breaks = round(seq(min(plot_data_melt2$ketvirtis),max(plot_data_melt2$ketvirtis), by = 1),0)) +
  ylab("Eur") +
  xlab("Ketvirtis") +
  ggtitle("EDP/Nuova kuro sąnaudos 100 km") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24),
        legend.text=element_text(size=13))

# Kuro sąnaudos vs krovinio svoris, skirtumas tarp euro 5 ir euro 6 ----------------------------------------

data$vid_pakr_t <- as.numeric(data$vid_pakr_t)
data <- left_join(data,masina_modelis, by = c("masina" = "Numeris"))
kuras_masina <- aggregate(lt_100km_faktas ~ masina, data = data, FUN = mean)
tonazas_masina <- aggregate(vid_pakr_t ~ masina, data = data, FUN = mean)
kuras_masina <- left_join(kuras_masina, tonazas_masina)
kuras_masina <- left_join(kuras_masina,data[,c("masina","Variklis")])
kuras_masina <- kuras_masina[!is.na(kuras_masina$Variklis),]

ggplot(kuras_masina, aes(x=vid_pakr_t, y=lt_100km_faktas, color = Variklis)) +
  geom_point() +  
  geom_text(label = kuras_masina$masina) +
  geom_smooth(method = "lm") +
  ylab("Kuro sąnaudos 100 km litrais") +
  xlab("Vidutinis pakrovimas tonomis") +
  ggtitle("Kuro sąnaudų sąryšis su tonažu, skirtumas tarp Euro 5 ir Euro 6 variklių")

# Kuro sąnaudos vs krovinio svoris, skirtumas tarp modelių ----------------------------------------

kuras_masina <- aggregate(lt_100km_faktas ~ masina, data = data, FUN = mean)
tonazas_masina <- aggregate(vid_pakr_t ~ masina, data = data, FUN = mean)
kuras_masina <- left_join(kuras_masina, tonazas_masina)
kuras_masina <- left_join(kuras_masina,data[,c("masina","Modelis")])
kuras_masina <- kuras_masina[!is.na(kuras_masina$Modelis),]

ggplot(kuras_masina, aes(x=vid_pakr_t, y=lt_100km_faktas, color = Modelis)) +
  geom_point() +  
  geom_text(label = kuras_masina$masina) +
  geom_smooth(method = "lm") +
  ylab("Kuro sąnaudos 100 km litrais") +
  xlab("Vidutinis pakrovimas tonomis") +
  ggtitle("Kuro sąnaudų sąryšis su tonažu, skirtumas tarp Euro 5 ir Euro 6 variklių")


# Kuro sunaudojimas pagal klientą ir variklio tipą ------------------------

# skaičiuojam kuro sąnaudas litrais kiekvienam klientui metams
kuras_litrais_klientas_metai <- aggregate(lt_100km_faktas ~ klientas + Variklis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + Variklis, data = data, FUN = length)
kuras_litrais_klientas_metai <- left_join(kuras_litrais_klientas_metai,reisai_klientas)
kuras_litrais_klientas_metai$lt_100km_faktas2 <- kuras_litrais_klientas_metai$lt_100km_faktas / 
            as.numeric(kuras_litrais_klientas_metai$is_viso_dienu)

#metinis kuro išlaidų kilometrui grafikas
kuras_litrais_plot <- kuras_litrais_klientas_metai
ggplot(data = kuras_litrais_plot, aes(x = reorder(klientas, -lt_100km_faktas2), y = lt_100km_faktas2,
                                      group = Variklis)) +
  geom_col(aes(fill = Variklis) ,position = position_dodge()) +
  scale_y_continuous(breaks = round(seq(0, max(kuras_litrais_plot$lt_100km_faktas2), by = 5),1)) +
  ylab("Litrai") + 
  ggtitle("Kuro sąnaudos 100km faktas 2018 pagal klientą ir variklio tipą") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(lt_100km_faktas2,1)), size = 5)

# Pelnas dienai EUR pagal klientus ir variklio tipą, 4 ketvirtis vs metai --------------------

#skaičiuojam pelną dienai paketvirčiui kiekvienam klientui
pelnas_klientas_ketvirtis <- aggregate(pelnas_dienai ~ klientas + ketvirtis + Variklis, data = data, FUN = sum)
reisai_klientas <- aggregate(is_viso_dienu ~ klientas + ketvirtis + Variklis, data = data, FUN = length)
pelnas_klientas_ketvirtis <- left_join(pelnas_klientas_ketvirtis,reisai_klientas)
pelnas_klientas_ketvirtis$pelnas_dienai2 <- pelnas_klientas_ketvirtis$pelnas_dienai/pelnas_klientas_ketvirtis$is_viso_dienu

#paskutinio ketvirčio pelno grafikas
pelnas_plot <- pelnas_klientas_ketvirtis[pelnas_klientas_ketvirtis$ketvirtis == ketvirtis,]
ggplot(data = pelnas_plot, aes(x = reorder(klientas, -pelnas_dienai2), y = pelnas_dienai2)) +
  geom_col(aes(fill = Variklis) ,position = position_dodge()) +
  scale_y_continuous(breaks = round(seq(0, max(pelnas_plot$pelnas_dienai2), by = 10),0)) +
  ylab("EUR") + 
  ggtitle("Pelnas dienai 4 ketvirtis 2018 pagal klientą ir variklio tipą") +
  xlab("Klientas") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24)) +
  geom_text(aes(label = round(pelnas_dienai2,0)), size = 5)


# Mašinos išvažiuotos dienos per metus -------------------------------------

dienos_per_metus <- aggregate(is_viso_dienu ~ masina, data = data, FUN = sum)
write.xlsx(dienos_per_metus,"Dienos per metus.xlsx")

# Technocolor frachto struktūra euro 6 vs euro 5 praėjęs ketvirtis-----------------------------------------

strukturai <- data[data$klientas == "Technocolor",c("is_viso_dienu","pelnas_pries_pap_islaidas","islaidos_vairams","islaidos_keliams",
                      "islaidos_kurui","Variklis","ketvirtis")]

strukturai <- melt(strukturai, id=c("Variklis","ketvirtis"))

strukturai_agg <- aggregate(value ~ variable + Variklis + ketvirtis, data = strukturai, FUN = sum)

dienos <- strukturai_agg[strukturai_agg$variable == "is_viso_dienu",]
colnames(dienos)[4] <- "is_viso_dienu"

strukturai_agg <- strukturai_agg[strukturai_agg$variable != "is_viso_dienu",]


strukturai_agg <- left_join(strukturai_agg,dienos[,c("is_viso_dienu","Variklis","ketvirtis")])

strukturai_agg$variable_dienai <- strukturai_agg$value / strukturai_agg$is_viso_dienu

strukturai_agg <- strukturai_agg[strukturai_agg$ketvirtis == ketvirtis,]

strukturai_agg <- strukturai_agg[ , !(names(strukturai_agg) == "ketvirtis")]

pelnas <- strukturai_agg[strukturai_agg$variable == "pelnas_pries_pap_islaidas",c("Variklis","variable_dienai")]
colnames(pelnas)[2] <- "pelnas_dienai2"

strukturai_agg <- left_join(strukturai_agg, pelnas)

ggplot(strukturai_agg, aes(y = variable_dienai, x = reorder(Variklis, -pelnas_dienai2))) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_col(aes(fill=variable), 
           col="black", 
           size=.1) +
  ylab("EUR") + 
  ggtitle("Frachto dienai struktūra 4 ketvirtis 2018 metai (išrikiuota pagal pelną)") +
  xlab("Klientas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),
        axis.text.y = element_text(angle = 0, hjust = 1,size=13),
        axis.title.y = element_text(size = rel(1.4), angle = 90),
        axis.title.x = element_text(size = rel(1.4), angle = 0),
        plot.title = element_text(size = 24))

# Įrašom duomenis į failus ------------------------------------------------

write.xlsx(data,"K2 ataskaita extra.xlsx")
write.xlsx(pok,"pok.xlsx")


# Koeficientų skaičiavimas ------------------------------------------------

koef <- plot_data[plot_data$ketvirtis == ketvirtis,c("pelnas_dienai","klientas")]
koef$koef <- round((100 + (koef$pelnas_dienai - 100)/4)/100,2)
