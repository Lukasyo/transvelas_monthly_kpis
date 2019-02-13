library(openxlsx)
library(janitor)
library(tidyverse)
library(lubridate)

pradzia <- as.Date("2019-01-01")
pabaiga <- as.Date("2019-01-31")

infotrans2019 <- src_mysql(dbname = "infot_transvelas_2019",
                           host = "192.168.0.244", 
                           port = 3306, 
                           user = "transvelas2019",
                           password = "du4jRGv946S3LB6u")

uzsakymu_statistika <- clean_names(as.data.frame(
  tbl(infotrans2019, "uzsakymu_statistika") %>%
    select("uzsakymo_id","max_sask_ist_israsymo_data","max_krov_pristatymo_data","max_sask_ist_apmoketi_iki","sask_ist_full_apmoketa_data","concat_sask_ist_sask_numeriai") %>%
    collect()))

#isimu, kur nera saskaitos nurodytos
uzsakymu_statistika <- uzsakymu_statistika[uzsakymu_statistika$concat_sask_ist_sask_numeriai != "",]

#panašu, kad krovinio datoj klaida - vietoj 2019 rašo 2018
uzsakymu_statistika$max_krov_pristatymo_data <- gsub("2018","2019",uzsakymu_statistika$max_krov_pristatymo_data)

# randam vėliausią įvykdytą užsakymą sąskaitai
max_krov_saskaitai <- aggregate(max_krov_pristatymo_data ~ concat_sask_ist_sask_numeriai, data = uzsakymu_statistika, FUN = max)
colnames(max_krov_saskaitai)[2] <- "veliausias_krov"
uzsakymu_statistika <- left_join(uzsakymu_statistika, max_krov_saskaitai)

#paliekam tik vieną eilutę vienai sąskaitai
uzsakymu_statistika <- uzsakymu_statistika[uzsakymu_statistika$max_krov_pristatymo_data == uzsakymu_statistika$veliausias_krov,]
uzsakymu_statistika <- uzsakymu_statistika[!duplicated(uzsakymu_statistika$concat_sask_ist_sask_numeriai),]

#saskaitos ir uzdaviniai, kurie neturi datos prie pristatymo
sask_uzd_be_krov_datos <- uzsakymu_statistika[uzsakymu_statistika$max_krov_pristatymo_data == "0000-00-00",]

#paliekam tik saskaitas su pilnais duomenimis
uzsakymu_statistika <- uzsakymu_statistika[!uzsakymu_statistika$uzsakymo_id %in% sask_uzd_be_krov_datos$uzsakymo_id,]

uzsakymu_statistika[uzsakymu_statistika$max_sask_ist_israsymo_data == "0000-00-00","max_sask_ist_israsymo_data"] <- as.character(as.POSIXct(Sys.Date()))

uzsakymu_statistika$max_sask_ist_israsymo_data <- as.POSIXct(uzsakymu_statistika$max_sask_ist_israsymo_data)
uzsakymu_statistika$max_krov_pristatymo_data <- as.POSIXct(uzsakymu_statistika$max_krov_pristatymo_data)

uzsakymu_statistika$nuo_iskr_iki_israsymo <- difftime(uzsakymu_statistika$max_sask_ist_israsymo_data,uzsakymu_statistika$max_krov_pristatymo_data, units = "days")
