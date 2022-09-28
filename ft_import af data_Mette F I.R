# Denne fil indhenter henter og rengør data fra FT referater (samler data til CSV).
# Referaterne er indhentet fra FT åbne server fra linket: "ftp://oda.ft.dk/ODAXML/Referat/"
# Linket er indsat i windows stifinder, hvorefter alle referater under Mette F er kopieret
# til egen mappe og importeret til R der fra.

require(tidyverse)
require(xml2)
library(lubridate)

my_files <- list.files("C:/Users/malth/OneDrive/Kodning/Folketinget/filer/Mette F 062019-062022", full.names = TRUE)

read_my_xml <- function(x) {
  doc <- read_xml(x)
  Tale <- doc %>% xml_find_all("DagsordenPunkt/Aktivitet/Tale")
  Dagsorden <- doc %>% xml_find_all("DagsordenPunkt")
  
  #ItemNo <- Dagsorden %>% xml_find_first("MetaFTAgendaItem/ItemNo") %>% xml_text()
  Casenr <- Dagsorden %>% xml_find_first("MetaFTAgendaItem/FTCaseNumber") %>% xml_text()
  #CaseType <- Dagsorden %>% xml_find_first("MetaFTAgendaItem/FTCaseType>") %>% xml_text()
  #CaseStage <- Dagsorden %>% xml_find_first("MetaFTAgendaItem/FTCaseStage>") %>% xml_text()
  Emne <- Dagsorden %>% xml_find_first("MetaFTAgendaItem/ShortTitle") %>% xml_text()
  Starttidspunkt1 <- Dagsorden %>% xml_find_first("Aktivitet/Tale/TaleSegment/MetaSpeechSegment/StartDateTime") %>% xml_text()
  
  Fornavn <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorFirstName") %>% xml_text()
  Efternavn <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorLastName") %>% xml_text()
  Partibogstav <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/GroupNameShort") %>% xml_text()
  Rolle <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorRole") %>% xml_text()
  Starttidspunkt2 <- Tale %>% xml_find_first("TaleSegment/MetaSpeechSegment/StartDateTime") %>% xml_text()
  Sluttidspunkt <- Tale %>% xml_find_first("TaleSegment/MetaSpeechSegment/EndDateTime") %>% xml_text()
  Type <- Tale %>% xml_find_first("TaleType/Linea/Char") %>% xml_text()
  
  
  dfs <- lapply(Tale, function(node){
    text <- node %>% xml_find_all(xpath='TaleSegment/TekstGruppe/Exitus/Linea/Char') %>% xml_text()
    text <- paste(text, collapse = ", ")
    data.frame(text)})
  
  #combine everything into 1 data frame
  df1 <- data.frame(Casenr, Emne, Starttidspunkt1)
  df1$Starttidspunkt <- df1$Starttidspunkt1
  df3 <- data.frame(Fornavn, Efternavn, Partibogstav, Rolle, Starttidspunkt2, Sluttidspunkt, Type, bind_rows(dfs))
  df3$Starttidspunkt <- df3$Starttidspunkt2
  df1$Starttidspunkt <- ymd_hms(df1$Starttidspunkt)
  df3$Starttidspunkt <- ymd_hms(df3$Starttidspunkt)
  
  #rbind(df3, df1)
  data_bind_rows <- bind_rows(df3, df1)
  data_bind_rows <- data_bind_rows %>%
    arrange(Starttidspunkt)
  data_bind_rows %>% fill(Casenr, Emne)

  #data_bind_rows$Starttidspunkt <- ymd_hms(data_bind_rows$Starttidspunkt)
}

dat <- map_df(my_files, read_my_xml)



dat$Starttidspunkt <- gsub("T", "-", dat$Starttidspunkt)
dat$Sluttidspunkt <- gsub("T", "-", dat$Sluttidspunkt)
dat$nwords <- str_count(dat$text, "\\w+")
dat$navn <- paste(dat$Fornavn, dat$Efternavn)
dat <- dat %>% relocate(navn)
dat <- dat[!(dat$Fornavn =="MødeSlut"),]
dat <- dat[!(dat$Fornavn ==""),]
dat <- dat[!(dat$Fornavn =="Pause"),]
dat <- dat[!(dat$Emne =="Punkt 0"),]
dat <- dat %>% drop_na(navn)
dat <- dat %>% drop_na(Emne)
dat$navn[dat$navn == "Kaare Dybvad"] <- "Kaare Dybvad Bek"
dat$navn[dat$navn == "Peter Hummelgaard Thomsen"] <- "Peter Hummelgaard"
dat$navn[dat$navn == "Aaja Chemnitz Larsen"] <- "Aaja Chemnitz"
dat$navn[dat$navn == "Lisbeth Bech Poulsen"] <- "Lisbeth Bech-Nielsen"

write.csv(dat,"C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MetteF_regering (06.2019 - 06.2022)", row.names = FALSE)

MF_baggrund <- read.csv("C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MF baggrund")

samlet <- merge(dat, MF_baggrund, by = "navn", all = TRUE)
samlet <- samlet %>% drop_na(text)


samlet$Partibogstav[samlet$Rolle == 'minister' |
                   samlet$Rolle == 'fungerende minister' |
                   samlet$Fuldenavn == 'Mette Frederiksen'] <- 'S'

samlet$Rolle[samlet$Rolle == 'fungerende minister'] <- 'minister'
samlet$Rolle[samlet$Fuldenavn == "Mette Frederiksen"] <- "minister"

samlet$Rolle[samlet$Rolle == 'midlertidig formand' |
                      samlet$Rolle == 'aldersformanden'] <- 'formand'

blok <- c("Rød", "Blå", "Rød", "Rød", "", "", "Blå", "Blå", "Blå", "",
          "Blå", "Rød", "Rød", "Rød", "", "", "", "Blå")
Partibogstav <- c("ALT", "DF", "EL", "FG", "IA", "JF", "KD", "KF", "LA", "M", 
                  "NB",  "RV", "S", "SF", "SIU", "SP",  "UFG", "V")
parti_blok <- data.frame(Partibogstav, blok)
samlet <- merge(samlet, parti_blok,by="Partibogstav")

samlet$Starttidspunkt <- ymd_hms(samlet$Starttidspunkt)
samlet$Sluttidspunkt <- ymd_hms(samlet$Sluttidspunkt)
samlet$difftid <- difftime(samlet$Sluttidspunkt, samlet$Starttidspunkt , units = "secs")

samlet$born <- as.Date(samlet$born, format = "%d-%m-%Y")
samlet$alder = as.numeric(difftime(Sys.Date(),samlet$born, units = "weeks"))/52.25

samlet <- samlet %>%
  select(navn, parti, Partibogstav, blok, køn, alder, text, nwords, difftid, Rolle, Type, Casenr, Emne, Starttidspunkt, Sluttidspunkt, id, born, uddannelsesniveau, Uddannelse, erhvervserf, ftstartdato, kreds_nu, kreds_tidligere, navn_parti, status, profession, biografi)

write.csv(samlet,"C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MetteF_regering (06.2019 - 06.2022)", row.names = FALSE)

