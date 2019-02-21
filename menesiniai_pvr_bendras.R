library(tidyverse)
library(RMySQL)
library(openxlsx)
library(lubridate)

pradzia <- as.Date("2019-01-01")
pabaiga <- as.Date("2019-01-31")
periodas <- seq(pradzia,pabaiga, by = "day")

# Set up a connection to the mysql database (metrics)
infotrans2019 <- src_mysql(dbname = "infot_transvelas_2019",
                           host = "192.168.0.244", 
                           port = 3306, 
                           user = "transvelas2019",
                           password = "du4jRGv946S3LB6u")


masinos <- as.data.frame(
  tbl(infotrans2019, "masinos") %>%
    collect())

keliones_lapas <- as.data.frame(
  tbl(infotrans2019, "keliones_lapas") %>%
    collect())

neskaiciuot_masinu <- read.csv("data/neskaiciuot_masinu.csv", stringsAsFactors = FALSE)

keliones_lapas <- keliones_lapas[!keliones_lapas$masina %in% neskaiciuot_masinu$masina,]

#paliekam tik baigtus
keliones_lapas <- keliones_lapas[keliones_lapas$baigtas == "1",]

# skaičiuojam kiek dirbta dienų per periodą
for (i in 1:nrow(keliones_lapas)) {
  
  dienos <- seq(keliones_lapas[i,"isvykimo_data"],keliones_lapas[i,"atvykimo_data"], by = "day")
  keliones_lapas[i,"dirbta_dienu_per_perioda"] <- sum(dienos %in% periodas)
  
}

# kiek kilometrų nuvažiuota per reikalingą periodą
keliones_lapas$nuvaziuota_per_perioda <- (keliones_lapas$spid_finis - keliones_lapas$spid_start) * keliones_lapas$dirbta_dienu_per_perioda / keliones_lapas$viso_dienu

# sunaudota kuro per periodą
keliones_lapas$sunaudota_kuro_per_perioda <- keliones_lapas$sunaudota_kuro_fakt_ltr * keliones_lapas$dirbta_dienu_per_perioda / keliones_lapas$viso_dienu

# kuro sunaudojimas per perioda pagal reisą
keliones_lapas$kuro_sunaudojimas_per_perioda <- keliones_lapas$sunaudota_kuro_per_perioda / keliones_lapas$nuvaziuota_per_perioda * 100

# vidutinis kuro sunaudojimas per periodą
sum(keliones_lapas$sunaudota_kuro_per_perioda) / sum(keliones_lapas$nuvaziuota_per_perioda) *100





