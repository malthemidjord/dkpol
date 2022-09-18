# Denne fil indhenter henter og rengør data fra FT referater (samler data til CSV).
# Referaterne er indhentet fra FT åbne server fra linket: "ftp://oda.ft.dk/ODAXML/Referat/"
# Linket er indsat i windows stifinder, hvorefter alle referater under Mette F er kopieret
# til egen mappe og importeret til R der fra.

require(tidyverse)
require(xml2)

my_files <- list.files("C:/Users/malth/OneDrive/Malthe/ft/filer/Lars L II 062015-112016", full.names = TRUE)

read_my_xml <- function(x) {
  doc <- read_xml(x)
  Tale <- doc %>% xml_find_all("DagsordenPunkt/Aktivitet/Tale")
  
  
  Fornavn <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorFirstName") %>% xml_text()
  Efternavn <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorLastName") %>% xml_text()
  Partibogstav <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/GroupNameShort") %>% xml_text()
  Rolle <- Tale %>% xml_find_first("Taler/MetaSpeakerMP/OratorRole") %>% xml_text()
  Starttidspunkt <- Tale %>% xml_find_first("TaleSegment/MetaSpeechSegment/StartDateTime") %>% xml_text()
  Sluttidspunkt <- Tale %>% xml_find_first("TaleSegment/MetaSpeechSegment/EndDateTime") %>% xml_text()
  
  
  dfs <- lapply(Tale, function(node){
    text <- node %>% xml_find_all(xpath='TaleSegment/TekstGruppe/Exitus/Linea/Char') %>% xml_text()
    text <- paste(text, collapse = ", ")
    data.frame(text)})
  
  #combine everything into 1 data frame
  df3 <- data.frame(Fornavn, Efternavn, Partibogstav, Rolle, Starttidspunkt, Sluttidspunkt, bind_rows(dfs))
  
}

dat <- map_df(my_files, read_my_xml)



dat$Starttidspunkt <- gsub("T", "-", dat$Starttidspunkt)
dat$Sluttidspunkt <- gsub("T", "-", dat$Sluttidspunkt)
dat$nwords <- str_count(dat$text, "\\w+")
dat$Fuldenavn <- paste(dat$Fornavn, dat$Efternavn)
dat <- dat %>% relocate(Fuldenavn)
dat <- dat[!(dat$Fornavn =="MødeSlut"),]
dat <- dat[!(dat$Fornavn ==""),]
dat <- dat[!(dat$Fornavn =="Pause"),]


navn <- unique(dat$Fuldenavn)
unikkenavne_lars_II <- data.frame(navn)
write.csv(unikkenavne_lars_II, "C:/Users/malth/OneDrive/Malthe/ft/Data/unikkenavne_lars_II", row.names = FALSE)

navn_køn <- read.csv("C:/Users/malth/OneDrive/Malthe/ft/Data/navn_kn_lars II.csv", sep = ";")
dat <- merge(dat,navn_køn,by="Fuldenavn")
dat$Partibogstav[dat$Rolle == 'minister' |
                    dat$Rolle == 'fungerende minister' |
                    dat$Fuldenavn == 'Lars Løkke Rasmussen'] <- 'V'
dat$Rolle[dat$Rolle == 'fungerende minister'] <- 'minister'



blok <- c("", "", "Blå", "Blå", "Blå", "Blå", "Rød", "Rød", "", "Rød",
          "Rød", "Rød", "", "", "")

Partibogstav <- c("UFG", "SIU", "DF",  "KF",  "V",   "LA",  "RV",  "S",
                  "",    "ALT", "EL",  "SF",  "T",   "JF",  "IA")


parti_blok <- data.frame(Partibogstav, blok)
dat <- merge(dat, parti_blok,by="Partibogstav")

write.csv(dat,"C:/Users/malth/OneDrive/Malthe/ft/Data/Lars L regering II (06.2015 - 11.2016)", row.names = FALSE)
