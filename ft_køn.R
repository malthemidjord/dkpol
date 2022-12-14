require(tidyverse)
library(stringr)
library(lubridate)


setwd("C:/Users/malth/OneDrive/Malthe/ft")

MetteF_I <- read.csv("Data/MetteF_regering (06.2019 - 06.2022)")
LarsL_II <- read.csv("Data/Lars L regering II (06.2015 - 11.2016)")
LarsL_III <- read.csv("Data/Lars L regering III (11.2016 - 06.2019)")
total$Rolle[total$Rolle == "midlertidig formand" | total$Rolle == "aldersformanden"] <- "formand"

LarsL_II$Regering <- 'Lars L II'
LarsL_III$Regering <- 'Lars L III'
MetteF_I$Regering <- 'Mette F I'

total <- rbind(MetteF_I, LarsL_II, LarsL_III)

total$Starttidspunkt <- ymd_hms(total$Starttidspunkt)
total$Sluttidspunkt <- ymd_hms(total$Sluttidspunkt)
total$difftid <- difftime(total$Sluttidspunkt, total$Starttidspunkt , units = "secs")
total$k?n[total$k?n == "0"] <- "Mand"
total$k?n[total$k?n == "1"] <- "Kvinde"

## Plot k?n generelt
k1 <- total %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Regering == "Mette F I") %>%
  group_by(year = lubridate::floor_date(Starttidspunkt, 'year'), k?n) %>%
  summarise(antal_taler = n())

k1 <- k1 %>%
  group_by(k?n) %>%
  summarise(gns = mean(antal_taler))

ggplot(k1, aes(x= k?n, y = gns, fill = k?n)) +
  geom_bar(stat="identity", width=0.5)+
  theme_minimal()+scale_fill_manual(values=c("#E69F00", "#009E73"))+
  geom_text(aes(label = round(gns, 0)), vjust = 2, size = 5, colour = "white", fontface = "bold")+
  labs(
    title = "?rlige antal taler i Folketinget fordelt p? k?n",
    caption = "Note: I Mette F regeringstid",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=8), legend.position = 'none')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

ggsave("C:/Users/malth/OneDrive/Malthe/ft/k?n/k1.png", width=3.5, height=2.5, bg="white")


## Gennemsnitlige antal taler per politiker fordelt p? k?n
k2 <- total %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Regering == "Mette F I") %>%
  group_by(year = lubridate::floor_date(Starttidspunkt, 'year'), Fuldenavn, k?n) %>%
  summarise(antal_taler = n())

k2 <- k2 %>%
  group_by(k?n) %>%
  summarise(gns = mean(antal_taler))

ggplot(k2, aes(x= k?n, y = gns, fill = k?n)) +
  geom_bar(stat="identity", width=0.5)+
  theme_minimal()+scale_fill_manual(values=c("#E69F00", "#009E73"))+
  geom_text(aes(label = round(gns, 0)), vjust = 2, size = 5, colour = "white", fontface = "bold")+
  labs(
    title = "Politikeres gennemsnitlige antal taler per ?r
    i FT fordelt p? k?n",
    caption = "Note: I Mette F regeringstid.
    Tager h?jde for k?nssammens?tningen i FT.",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=8), legend.position = 'none')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

ggsave("C:/Users/malth/OneDrive/Malthe/ft/k?n/k2.png", width=3, height=2.5, bg="white")


### Fordeling
k3 <- total %>%
  filter(Rolle == "medlem") %>%
  filter(Regering == "Mette F I") %>%
  group_by(Fuldenavn, k?n) %>%
  summarise(antal_taler = n())


ggplot(k3, aes(x = antal_taler, fill = k?n)) + 
  geom_density(alpha = 0.6,color=NA)+scale_fill_manual(values=c("#E69F00", "#009E73"),
                                                       name="K?n")+
  theme_minimal()+
  labs(
    title = "Fordelingen af politikere efter antal af taler
      i FT fordelt p? k?n",
    caption = "Note: I Mette F's regeringstid",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=10))+
  theme(plot.title = element_text(size=10),
        legend.position = c(0.87, 0.7),
        legend.title = element_blank())

ggsave("C:/Users/malth/OneDrive/Malthe/ft/k?n/k3.png", width=4, height=2.5, bg="white")


## Mellem regeringer
k4 <- total %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>% 
  group_by(year = lubridate::floor_date(Starttidspunkt, 'year'), Regering, Fuldenavn, k?n) %>%
  summarise(antal_taler = n())

k4 <- k4 %>%
  group_by(Regering, k?n) %>%
  summarise(gns = mean(antal_taler))

ggplot(k4, aes(x= Regering, y = gns, fill=k?n)) +
  geom_bar(stat="identity", position=position_dodge(width = .9))+
  theme_minimal()+scale_fill_manual(values=c("#E69F00", "#009E73"),
                                    name="K?n")+
  geom_text(aes(label = round(gns, 0)), position = position_dodge(width = .9),
            vjust = 2, size = 5, colour = "white", fontface = "bold")+
  geom_text(x=3, y= 35, label = "Forskel:", colour = "red", size = 3.5)+
  geom_text(x=3, y= 25, label = "24 pct.", colour = "red", size = 3.5)+
  geom_text(x=2, y= 35, label = "Forskel:", colour = "red", size = 3.5)+
  geom_text(x=2, y= 25, label = "13 pct.", colour = "red", size = 3.5)+
  geom_text(x=1, y= 35, label = "Forskel:", colour = "red", size = 3.5)+
  geom_text(x=1, y= 25, label = "25 pct.", colour = "red", size = 3.5)+
  labs(
    title = '?rlige antal taler i FT fordelt p? regering og k?n',
    caption = 'Note: Lars L II (06.15 - 11.16), 
      Lars L III (11.16 - 06.19), 
      Mette F I (06.19 - ??.?? )',
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=10))

ggsave("C:/Users/malth/OneDrive/Malthe/ft/k?n/k4.png", width = 4, height = 3, bg="white")

##Taletid

k5 <- total %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Regering == "Mette F I") %>%
  group_by(k?n) %>%
  summarise(taletid = mean(difftid))

ggplot(k5, aes(x= k?n, y = taletid, fill = k?n)) +
  geom_bar(stat="identity", width=0.5)+
  theme_minimal()+scale_fill_manual(values=c("#E69F00", "#009E73"))+
  geom_text(aes(label = round(taletid, 0)), vjust = 2, size = 5, colour = "white", fontface = "bold")+
  labs(
    title = "Gennemsnitlig taletid per tale i FT fordelt p? k?n (sekunder)",
    caption = "Note: I Mette F regeringstid",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=8), legend.position = 'none')+
  coord_cartesian(ylim=c(0,90))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

ggsave("C:/Users/malth/OneDrive/Malthe/ft/k?n/k5.png", width=3.5, height=2.5, bg="white")



  