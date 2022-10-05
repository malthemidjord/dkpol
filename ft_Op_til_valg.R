library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggrepel)

d <- read.csv("C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MetteF_regering (06.2019 - 06.2022)")

partifarver <- list("S" = "#CC0000",
                    "LA" = "#00CCCC", "RV" = "#FF00CC", "M" = "#660099",
                    "SF" = "#990000", "EL" = "#993333", "NB" = "#336666", "V" = "#3399CC",
                    "KF" = "#006600", "ALT" = "#00FF33", "DF" = "#FFFF00", "FG" = "#FF9966",
                    "gns" = "darkgrey", "gns. parti" = "darkgrey")

pb <- "LA"
farve <- "white"


d1 <- d

d1 <- d1[!grepl("statsministerens åbningsredegørelse", d1$Emne),]
d1 <- d1[!grepl("indenrigs- og udenrigspolitiske situation", d1$Emne),]
d1 <- d1[!grepl("finanslov for finansåret", d1$Emne),]
d1 <- d1[!grepl("partilederdebat", d1$Emne),]
d1 <- d1[!grepl("Partilederdebat", d1$Emne),]
d1 <- d1[!grepl("Spørgetime med statsministeren", d1$Emne),]


p1 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == pb) %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(p1[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(antal_min, 0), "min")), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = paste("FT-debatter hvor MF'er fra", pb, "samlet har talt i 
    længst tid (tal = minutter)"),
    caption = paste("Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet talt.
                    ", pb, "har altså talt i", round(p1$antal_min[1],0), "min i førstnævnte debat"),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave(device= png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p1",pb,".png")), width = 4, height = 4, bg="white")


p21 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  group_by(Emne) %>%
  summarise(antal_min = round(sum(difftid)/60,2))

p22 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == pb) %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2))

p23 <- merge(p21, p22, by = "Emne")  

p23 <- p23 %>%
  filter(antal_min.x > 15) %>%
  mutate(andel = (antal_min.y/antal_min.x)*100) %>%
  filter(andel != 100) %>%
  arrange(desc(andel))


ggplot(p23[1:5,], aes(x = andel, y= reorder(Emne, +andel), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(andel, 0), "pct.")), hjust = 1.2, size = 3, colour = farve, fontface = "bold")+
  labs(
    title = paste("FT-debatter hvor", pb, "har den største andel taletid 
    af den samlede taletid"),
    caption = paste("Note: Tal i kolonner angiver partiets procentvise andel af taletiden
                    i den pågældende debat",
                    pb, "har således talt", round(p23$andel[1],0), "pct. af den sam-
                    lede taletid i førstnævnte debat. Debatter under 15 min. er fjernet."),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')

ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p2",pb,".png")), width = 4, height = 4, bg="white")




#De fem debatter, hvor der er talt mest, men hvor partiet ikke har deltaget

p24 <- merge(p21, p22, by = "Emne", all = TRUE)

p24 <- p24[is.na(p24$Partibogstav),]
p24$Partibogstav <- pb

p24 <- p24 %>%
  arrange(desc(antal_min.x))

ggplot(p24[1:5,], aes(x = antal_min.x, y= reorder(Emne, +antal_min.x), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(antal_min.x, 0), "min")), hjust = 1.2, size = 3, colour = farve, fontface = "bold")+
  labs(
    title = paste("FT-debatter med længst samlet taletid, 
                  men som", pb, "ikke har deltaget i"),
    caption = paste("Note: Oversigten viser debatter i FT med den længste samlede taletid,
    hvor MF'er fra", pb, "ikke deltaget. Tal i kolonner angiver samlet taletid i 
                    den gågældende debat. I den førstnævnte debat har der været talt i
                    ", round(p24$antal_min.x[1],0), "minutter uden", pb," har deltaget"),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')

ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p3",pb,".png")), width = 4, height = 4, bg="white")



#Hvilken MF'er taler mest?
p31 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == pb) %>%
  group_by(navn_parti, Partibogstav) %>%
  summarise(antal_taler = n()) %>%
  arrange(desc(antal_taler))

ggplot(p31[1:3,], aes(x = antal_taler, y= reorder(navn_parti, +antal_taler), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(antal_taler, 0), "taler")), hjust = 1.2, size = 3, colour = farve, fontface = "bold")+
  labs(
    title = paste("Rangering af MF'er fra", pb, " efter hvor 
                  mange gange de har talt i FT"),
    caption = paste("Note: Oversigten viser de MF'er fra", pb,"som har talt mest
    i FT under Mette F samt hvor mange gange de har talt.
                    ", p31$navn_parti[1], "har talt", round(p31$antal_taler[1]), "gange under Mette F"),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')
  
ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p4",pb,".png")), width = 4, height = 4, bg="white")



# Taler partiets MF'er mere end gns. MF'er?

p41 <- d1 %>%
  filter(Rolle == "medlem"| Rolle == "minister") %>%
  group_by(navn) %>%
  summarise(antal_taler = n())

p42 <- p41 %>%
  summarise(gns = mean(antal_taler))
  
p43 <- d1 %>%
  filter(Rolle == "medlem"| Rolle == "minister") %>%
  filter(Partibogstav == pb) %>%
  group_by(navn) %>%
  summarise(antal_taler = n())

p44 <- p43 %>%
  summarise(gns = mean(antal_taler))

titel <- c("Gns. i Folketinget", paste("Gns. i", pb))
værdi <- c(p42$gns[1], p44$gns[1])
Partibogstav <- c("gns", pb)

p45 <- data.frame(titel,værdi, Partibogstav)


ggplot(p45, aes(x = reorder(titel, -værdi), y= værdi, fill= Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(værdi, 0), "taler")), vjust = 1.2, size = 3, colour = farve, fontface = "bold")+
  labs(
    title = paste("Antal taler i FT fra den gennemsnitlige 
                  MF'er fra", pb, "og MF'er generelt"),
    caption = paste("Note: I Mette F's regeringsperiode har den 
                    gennemsnitlige MF'er fra", pb,"talt", round(p44$gns[1], 0), "gange"),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')

ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p5",pb,".png")), width = 4, height = 3, bg="white")




#Partiets lixtal

## Udregning af Lix
lix <- d1
lix$sætninger <- str_count(lix$text, regex("\\.|\\?|\\!", ignore_case = TRUE))
lix$sætninger[lix$sætninger == 0] <- 1
lix$lange_ord <- str_count(lix$text, regex('\\w{7,}', ignore_case = TRUE))

lix <- lix %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(nwords > 50) %>%
  mutate(LIX = (nwords/sætninger) + ((lange_ord*100)/nwords))

p51 <- lix %>%
  group_by(Partibogstav) %>%
  summarise(lix = mean(LIX))

p52 <- p51 %>%
  summarise(lix = mean(lix))


p53 <- lix %>%
  filter(Partibogstav == pb) %>%
  group_by(Partibogstav) %>%
  summarise(lix = mean(LIX))

p54 <- p53 %>%
  summarise(lix = mean(lix))

titel <- c("Folketinget generelt", pb)
værdi <- c(p52$lix[1], p54$lix[1])
Partibogstav <- c("gns", pb)

p55 <- data.frame(titel, værdi, Partibogstav)

ggplot(p55, aes(x = reorder(titel, -værdi), y= værdi, fill= Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = paste(round(værdi, 0), "lixtal")), vjust = 1.2, size = 3, colour = farve, fontface = "bold")+
  labs(
    title = paste("Lixtal for", pb, "og partierne generelt, når de taler i FT"),
    caption = paste("Note: I Mette F's regeringsperiode."),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')

ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p6",pb,".png")), width = 4, height = 3, bg="white")




#Udvikling over taler gennem regeringsperiode (SÆT DEN EVT FØR HVEM SOM TALER MEST)
d1$Starttidspunkt <- ymd_hms(d1$Starttidspunkt)

p61 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == pb) %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), Partibogstav) %>%
  summarise(antal_taler = n()) %>%
  arrange(week)

p62 <- d1 %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav != "IA" |Partibogstav != "JF" | Partibogstav != "SIU" |
           Partibogstav != "SP" | Partibogstav != "UFG") %>%
  group_by(week = lubridate::floor_date(Starttidspunkt, 'week'), Partibogstav) %>%
  summarise(antal_taler = n()) %>%
  arrange(week)

p63 <- p62 %>%
  group_by(week) %>%
  summarise(antal_taler = round(mean(antal_taler), 0))

p63$Partibogstav <- "gns. parti"

p64 <- rbind(p61, p63)

data_ends <- p64 %>% filter(as.character(week) == "2022-06-05")

            
p64 %>%
  ggplot(aes(x=week, y = antal_taler, color = Partibogstav, fill = Partibogstav)) + geom_smooth(se=F)+geom_point(alpha = 0.2)+
  theme_minimal()+ scale_color_manual(values=partifarver)+
    geom_text_repel(
      aes(label = Partibogstav), data = data_ends, color = "black", size = 4, fontface = "bold", nudge_y = -5
    )+
  labs(
    title = paste("Udvikling i antallet af ugentlige taler fra", pb, "
                  i folketinget"),
    caption = paste("Note: I Mette F's regeringsperiode."),
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none')

ggsave(device = png, gsub(" ", "", paste("C:/Users/malth/OneDrive/Kodning/Folketinget/Partier_op_til_valg/",pb,"/p7",pb,".png")), width = 4, height = 3, bg="white")



