library(openxlsx)
library(janitor)
library(tidyverse)

# neigimai parazitiniai kilometrai reiškia išlošti kilometrai; kuo žemesnis skaičius, tuo geresnis rezultatas; 
# jeigu žemiau šito skaičiaus, laikysim, kad nerealu ir kad tai klaida
leistina_paklaida <- -5
leistina_paklaida_min <- 50 #kiek procentų leidžiam maksimaliai parazitinių, visus kitus vertinam, kad klaida

pradzia <- as.Date("2019-01-01")
pabaiga <- as.Date("2019-01-31")
periodas <- seq(pradzia,pabaiga, by = "day")

data <- clean_names(read.xlsx("data/marsrutu_km_analize.xlsx"))

infotrans2019 <- src_mysql(dbname = "infot_transvelas_2019",
                           host = "192.168.0.244", 
                           port = 3306, 
                           user = "transvelas2019",
                           password = "du4jRGv946S3LB6u")

keliones_lapas <- clean_names(as.data.frame(
  tbl(infotrans2019, "keliones_lapas") %>%
    collect()))

skyriai <- clean_names(as.data.frame(
  tbl(infotrans2019, "skyriai") %>%
    collect()))

route <- clean_names(as.data.frame(
  tbl(infotrans2019, "route") %>%
    collect()))

st_uzsakymai <- clean_names(as.data.frame(
  tbl(infotrans2019, "st_uzsakymai") %>%
    collect()))

users <- clean_names(as.data.frame(
  tbl(infotrans2019, "users") %>%
    collect()))

# išimam maršrutus su nenustatyta pristatymo data
route <- route[route$route_pristatymo_data != "0000-00-00 00:00:00",]

# keičiam formatą datos
route$route_pristatymo_data <- as.POSIXct(route$route_pristatymo_data)

# kokia užvakar diena
uzvakar <- as.POSIXct(Sys.Date())-2*60*60*24

# išimam maršrutus, kurių pristatymas naujesni nei užvakar
route <- route[route$route_pristatymo_data < uzvakar,]

# priduriam vadybininką prie parazitinių km skaičiavimo
route <- left_join(route,users[,c("id","user")],by = c("user_id"="id"))

# skaičiuojam vadybininko suplanuotus kilometrus
route$vadybininko_suplanuoti_km <- route$route_krauti_km + route$route_tusti_km

# švarūs duomenys
clean_route <- route[,c("user","route_pavadinimas","route_odo_km","vadybininko_suplanuoti_km")]

# kiekvieno maršruto parazitiniai
clean_route$parazitiniai_proc <- (clean_route$route_odo_km/clean_route$vadybininko_suplanuoti_km - 1)*100

# išimam maršrutus, kur nerealūs rezultatai
clean_route <- clean_route[which(clean_route$parazitiniai_proc > leistina_paklaida),]
clean_route <- clean_route[which(clean_route$parazitiniai_proc < leistina_paklaida_min),]

# suma kiek suplanuota vadybiniko iš viso
suplanuoti <- aggregate(vadybininko_suplanuoti_km ~ user, data = clean_route, FUN = sum)

# suma kiek nuvažiuota vadybiniko iš viso
nuvaziuoti <- aggregate(route_odo_km ~ user, data = clean_route, FUN = sum)

# parazitinių km procentas pagal vadybininką
vadybininko_parazitiniai <- left_join(suplanuoti,nuvaziuoti)
vadybininko_parazitiniai$parazitiniai_proc <- (vadybininko_parazitiniai$route_odo_km/vadybininko_parazitiniai$vadybininko_suplanuoti_km - 1)*100

# spausdinam
write.xlsx(vadybininko_parazitiniai, file = "output/vadybininko_parazitiniai.xlsx")
