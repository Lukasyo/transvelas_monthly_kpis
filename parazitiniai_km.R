library(openxlsx)
library(janitor)
library(tidyverse)

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

data <- left_join(data,keliones_lapas[,c("reiso_nr","skyrius")])
