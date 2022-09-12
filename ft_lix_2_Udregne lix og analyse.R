require(tidyverse)
library(stringr)
library(lubridate)


setwd("C:/Users/malth/OneDrive/Malthe/ft")

#Importerer og klargør data
data <- read.csv("MetteF_regering (06.2019 - 06.2022)")

data$Starttidspunkt <- ymd_hms(data$Starttidspunkt)
data$Sluttidspunkt <- ymd_hms(data$Sluttidspunkt)
data$difftid <- difftime(data$Sluttidspunkt, data$Starttidspunkt , units = "secs")
navn_køn <- read.csv("navn_kn.csv", sep = ";")
data <- merge(data,navn_køn,by="Fuldenavn")
data$Fuldenavn[data$Fuldenavn == "Kaare Dybvad"] <- "Kaare Dybvad Bek"
data <- select(data, -n)
data$Partibogstav[data$Rolle == 'minister' |
                    data$Rolle == 'fungerende minister' |
                    data$Fuldenavn == 'Mette Frederiksen'] <- 'S'
data$Rolle[data$Rolle == 'fungerende minister'] <- 'minister'
data$Rolle[data$Fuldenavn == "Mette Frederiksen"] <- "minister"


# Assigner blokke til partier
blok <- c("Rød", "Blå", "Rød", "Rød", "", "", "Blå", "Blå", "Blå", "",
          "Blå", "Rød", "Rød", "Rød", "", "", "", "Blå")
Partibogstav <- c("ALT", "DF", "EL", "FG", "IA", "JF", "KD", "KF", "LA", "M", 
                  "NB",  "RV", "S", "SF", "SIU", "SP",  "UFG", "V")
parti_blok <- data.frame(Partibogstav, blok)
data <- merge(data, parti_blok,by="Partibogstav")


## Udregning af Lix
data$sætninger <- str_count(data$text, regex("\\.|\\?|\\!", ignore_case = TRUE))
data$sætninger[data$sætninger == 0] <- 1
data$lange_ord <- str_count(data$text, regex('\\w{7,}', ignore_case = TRUE))

lix <- data %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(nwords > 50) %>%
  mutate(LIX = (nwords/sætninger) + ((lange_ord*100)/nwords))


## gns. for partier
partier <- lix %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG", Rolle == "medlem") %>%
  group_by(Partibogstav) %>%
  summarise(Gns_lixtal = mean(LIX), antal_taler = n()) %>%
  arrange(desc(Gns_lixtal))

partifarver <- list("S" = "#CC0000", "LA" = "#00CCCC", "RV" = "#FF00CC", "M" = "#660099",
                    "SF" = "#990000", "EL" = "#993333", "NB" = "#336666", "V" = "#3399CC",
                    "KF" = "#006600", "ALT" = "#00FF33", "DF" = "#FFFF00")

blokfarver <- list ("Rød" = "#CC0000", "Blå" = "#3399CC")


ggplot(partier, aes(x = reorder(Partibogstav, -Gns_lixtal), y = Gns_lixtal, fill = Partibogstav))+
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal()+
  geom_text(aes(label = round(Gns_lixtal, 1)), vjust = 2, size = 2.5)+
  labs(
    title = 'Partiers gennemsnitlige lix-tal i folketingssalen',
    caption = "I Mette F's regeringstid",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis())+
  theme(legend.position="none")+ scale_fill_manual(values=partifarver)


ggsave("C:/Users/malth/OneDrive/Malthe/ft/lix/lix.png", width = 4, height = 2.5, bg="white")


## Blokke over tid
blok_lix <- lix %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG") %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), blok) %>%
  summarise(lix = mean(LIX)) %>%
  arrange(week)

blok_lix<- blok_lix[!(blok_lix$blok == ""),]

ggplot(blok_lix, aes(x=week, y = lix, color = blok, fill = blok)) + geom_smooth()+geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(
    title = 'Gennemsnitligt lix-tal fordelt på blok over tid',
    caption = "Note: I Mette F's regeringstid. Kilde: FT åbne data",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=10))+
  coord_cartesian(ylim=c(20,50))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis())+
  theme(legend.position = c(0.87, 0.25), legend.title = element_blank())+ scale_color_manual(values=blokfarver)+
  scale_fill_manual(values=blokfarver)

blok_lix %>%
  group_by(blok) %>%
  summarise(mean(lix))

ggsave("C:/Users/malth/OneDrive/Malthe/ft/lix/lix2.png", width = 4, height = 2.5, bg="white")

## Minister og MF over tid
min_lix <- lix %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG", Starttidspunkt > "2019-08-23 00:00:00") %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), Rolle) %>%
  summarise(lix = mean(LIX)) %>%
  arrange(week)

ggplot(min_lix, aes(x=week, y = lix, color = Rolle, fill = Rolle)) + geom_smooth()+geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(
    title = 'Gennemsnitligt lix-tal for 
    minister og medlem af FT over tid',
    caption = "I Mette F's regeringstid. Kilde: FT åbne data",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=10),
                                 legend.position = c(0.87, 0.25),
                                 legend.title = element_blank())+
  coord_cartesian(ylim=c(20,50))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis())+
  scale_color_manual(values= c("#FF9933", "#663399"))+
  scale_fill_manual(values= c("#FF9933", "#663399"))



ggsave("C:/Users/malth/OneDrive/Malthe/ft/lix/lix3.png", width = 4, height = 2.5, bg="white")

## TESTER

fulde <- lix %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG") %>%
  group_by(Fuldenavn, Partibogstav) %>%
  summarise(lix = mean(LIX), antal_taler = sum(nwords)) %>%
  arrange(week)

ggplot(fulde, aes(y=antal_taler, x = lix, color = Fuldenavn))+geom_point()+
  theme(legend.position="none")


lix %>%
  filter(Fuldenavn == "Morten Messerschmidt") %>%
  group_by(Fuldenavn, Partibogstav) %>%
  summarise(Gns_lixtal = mean(LIX), antal_taler = n()) %>%
  filter(antal_taler > 50) %>%
  arrange(desc(Gns_lixtal))



lix_tid <- lix %>%
  filter(Starttidspunkt > "2020-10-06 00:00:00", Starttidspunkt < "2022-11-30 00:00:00" ) %>%
  #filter(Partibogstav == "DF" | Partibogstav == "RV") %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG")  %>%
  #filter(blok == "Rød" | blok == "Blå") %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), Partibogstav) %>%
  summarise(lix = mean(LIX)) %>%
  arrange(week)


lix_tid <- lix %>%
  filter(Starttidspunkt > "2022-01-15 00:00:00", Starttidspunkt < "2022-06-23 00:00:00") %>%
  filter(Fuldenavn == "Christian Rabjerg Madsen") %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), Fuldenavn) %>%
  summarise(lix = mean(LIX)) %>%
  arrange(week)


ggplot(lix_tid, aes(x = week, y = lix))+geom_col()


lix %>%
  filter(Partibogstav != "IA", Partibogstav != "JF", Partibogstav != "SIU",
         Partibogstav != "SP", Partibogstav != "UFG", Partibogstav != "KD",
         Partibogstav != "FG", Rolle == "medlem") %>%
  group_by(Partibogstav) %>%
  summarise(Gns_lixtal = mean(LIX), antal_taler = n()) %>%
  arrange(desc(Gns_lixtal))
