# Denne fil indhenter henter og reng?r data fra FT referater (samler data til CSV).
# Referaterne er indhentet fra FT ?bne server fra linket: "ftp://oda.ft.dk/ODAXML/Referat/"
# Linket er indsat i windows stifinder, hvorefter alle referater under Mette F er kopieret
# til egen mappe og importeret til R der fra.

require(tidyverse)
require(xml2)

my_files <- list.files("C:/Users/malth/OneDrive/Malthe/ft/filer/Lars L III 112016-062019", full.names = TRUE)

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
dat <- dat[!(dat$Fornavn =="M?deSlut"),]
dat <- dat[!(dat$Fornavn ==""),]
dat <- dat[!(dat$Fornavn =="Pause"),]


navn <- unique(dat$Fuldenavn)
unikkenavne_lars_III <- data.frame(navn)
write.csv(unikkenavne_lars_III, "C:/Users/malth/OneDrive/Malthe/ft/Data/unikkenavne_lars_III", row.names = FALSE)

navn_k?n <- read.csv("C:/Users/malth/OneDrive/Malthe/ft/Data/navn_kn_lars III.csv", sep = ";")
dat <- merge(dat,navn_k?n,by="Fuldenavn")
dat$Rolle[dat$Rolle == 'fungerende minister'] <- 'minister'

dat$Partibogstav[dat$Fuldenavn == "Anders Samuelsen" | 
                 dat$Fuldenavn == "Merete Riisager" |
                 dat$Fuldenavn == "Mette Bock" |
                 dat$Fuldenavn == "Ole Birk Olesen" |
                 dat$Fuldenavn == "Thyra Frank"] <- 'LA'


dat$Partibogstav[dat$Fuldenavn == "Brian Mikkelsen"|
                   dat$Fuldenavn == "Mai Mercado"|
                   dat$Fuldenavn =="Rasmus Jarlov"|
                   dat$Fuldenavn == "S?ren Pape Poulsen"] <- "KF"

dat$Partibogstav[dat$Fuldenavn == "Claus Hjort Frederiksen" |
                   dat$Fuldenavn =="Ellen Trane N?rby" |
                   dat$Fuldenavn =="Esben Lunde Larsen"|
                   dat$Fuldenavn =="Eva Kjer Hansen"|
                   dat$Fuldenavn =="Inger St?jberg"|
                   dat$Fuldenavn =="Jakob Ellemann-Jensen"|
                   dat$Fuldenavn =="Karen Ellemann"|
                   dat$Fuldenavn =="Karsten Lauritzen"|
                   dat$Fuldenavn =="Kristian Jensen"|
                   dat$Fuldenavn =="Lars Christian Lilleholt"|
                   dat$Fuldenavn =="Lars L?kke Rasmussen"| 
                   dat$Fuldenavn =="Sophie L?hde"|
                   dat$Fuldenavn =="S?ren Pind"|
                   dat$Fuldenavn =="Tommy Ahlers"|
                   dat$Fuldenavn =="Troels Lund Poulsen"] <- "V"




blok <- c("", "", "Bl?", "Bl?", "Bl?", "Bl?", "R?d", "R?d", "", "R?d",
          "R?d", "R?d", "", "", "")

Partibogstav <- c("UFG", "NQ", "DF",  "KF",  "V",   "LA",  "RV",  "S",
                  "",    "ALT", "EL",  "SF",  "T",   "JF",  "IA")


parti_blok <- data.frame(Partibogstav, blok)
dat <- merge(dat, parti_blok,by="Partibogstav")

write.csv(dat,"C:/Users/malth/OneDrive/Malthe/ft/Data/Lars L regering III (11.2016 - 06.2019)", row.names = FALSE)
