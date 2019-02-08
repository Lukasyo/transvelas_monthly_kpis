library(tidyverse)
library(RMySQL)
library(openxlsx)

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

keliones_lapas$isvykimo_data <- as.Date(keliones_lapas$isvykimo_data)
keliones_lapas[keliones_lapas$atvykimo_data == "0000-00-00","atvykimo_data"] <- "1989-06-06"
keliones_lapas$atvykimo_data <- as.Date(keliones_lapas$atvykimo_data)
keliones_lapas[keliones_lapas$atvykimo_data == "1989-06-06","atvykimo_data"] <- Sys.Date()
keliones_lapas <- keliones_lapas[keliones_lapas$masina != "",]

for (i in 1:nrow(keliones_lapas)) {
  
  dienos <- seq(keliones_lapas[i,"isvykimo_data"],keliones_lapas[i,"atvykimo_data"], by = "day")
  keliones_lapas[i,"dirbta_dienu_per_perioda"] <- sum(dienos %in% periodas)

}

masinos_dirbo_dienu_per_perioda <- aggregate(dirbta_dienu_per_perioda ~ masina, data = keliones_lapas, FUN = sum)
masinos_dienos <- "test"

keliones_lapas$intervalas <- interval(keliones_lapas$isvykimo_data,keliones_lapas$atvykimo_data)

for ( m in unique(keliones_lapas$masina)) {
  
  reisai <- keliones_lapas[keliones_lapas$masina == m,"reiso_nr"]
  
  for (r in reisai){
    
    kiti_reisai <- reisai[!reisai == r]
    
    for (k in kiti_reisai){
      
      if(int_overlaps(keliones_lapas[keliones_lapas$reiso_nr == r,"intervalas"],keliones_lapas[keliones_lapas$reiso_nr == k,"intervalas"]) == TRUE){
        
        keliones_lapas[keliones_lapas$reiso_nr == r, "persikloja_periodai"] <- TRUE
        keliones_lapas[keliones_lapas$reiso_nr == k, "persikloja_periodai"] <- TRUE
        
      }
      
    }
    
  }
  
}

dubliuoti_reisai <- keliones_lapas[keliones_lapas$persikloja_periodai == TRUE & !is.na(keliones_lapas$persikloja_periodai),c("masina","reiso_nr")]

masinos_dirbo_dienu_per_perioda$procentu_atidirbta <- round(masinos_dirbo_dienu_per_perioda$dirbta_dienu_per_perioda / length(periodas) * 100,1)

masinos_dirbo_dienu_per_perioda <- masinos_dirbo_dienu_per_perioda[order(masinos_dirbo_dienu_per_perioda$dirbta_dienu_per_perioda, decreasing = TRUE),]

print("Periodo vidurkis (%):")
print(round(mean(masinos_dirbo_dienu_per_perioda$procentu_atidirbta),2))

write.xlsx(masinos_dirbo_dienu_per_perioda, "output/masinos_atidirbo.xlsx", asTable = TRUE)


