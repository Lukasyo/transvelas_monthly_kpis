library(tidyverse)

#pakraunam duomenis
dedal <- read.csv("data/dedal.csv", stringsAsFactors=FALSE)
eurolinen <- read.csv("data/eurolinen.csv", stringsAsFactors=FALSE)
technocolor <- read.csv("data/technocolor.csv", stringsAsFactors=FALSE)

#nurodom koks skyrius kurio dataframe
dedal$klientas <- "Dedal"
eurolinen$klientas <- "eurolinen"
technocolor$klientas <- "technocolor"
