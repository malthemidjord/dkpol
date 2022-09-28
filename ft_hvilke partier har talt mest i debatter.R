library(tidyverse)
library(dplyr)
library(stringr)

d <- read.csv("C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MetteF_regering (06.2019 - 06.2022)")

d <- d[!grepl("statsministerens åbningsredegørelse", d$Emne),]
d <- d[!grepl("indenrigs- og udenrigspolitiske situation", d$Emne),]
d <- d[!grepl("finanslov for finansåret", d$Emne),]
d <- d[!grepl("partilederdebat", d$Emne),]
d <- d[!grepl("Partilederdebat", d$Emne),]
d <- d[!grepl("Spørgetime med statsministeren", d$Emne),]

partifarver <- list("S" = "#CC0000",
                    "LA" = "#00CCCC", "RV" = "#FF00CC", "M" = "#660099",
                    "SF" = "#990000", "EL" = "#993333", "NB" = "#336666", "V" = "#3399CC",
                    "KF" = "#006600", "ALT" = "#00FF33", "DF" = "#FFFF00", "FG" = "#FF9966")


S <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "S") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(S[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra S har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/S.png", width = 4, height = 4, bg="white")


DF <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "DF") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(DF[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "black", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra DF har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/DF.png", width = 4, height = 4, bg="white")



LA <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "LA") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(LA[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra LA har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/LA.png", width = 4, height = 4, bg="white")


RV <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "RV") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(RV[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra RV har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/RV.png", width = 4, height = 4, bg="white")



SF <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "SF") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(SF[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra SF har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/SF.png", width = 4, height = 4, bg="white")




EL <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "EL") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(EL[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra EL har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/EL.png", width = 4, height = 4, bg="white")




NB <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "NB") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(NB[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra NB har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/NB.png", width = 4, height = 4, bg="white")



LA <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "LA") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(LA[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra LA har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/LA.png", width = 4, height = 4, bg="white")




KF <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "KF") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(KF[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra KF har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/KF.png", width = 4, height = 4, bg="white")



ALT <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "ALT") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(ALT[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "black", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra ALT har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/ALT.png", width = 4, height = 4, bg="white")



FG <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "FG") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(FG[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "black", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra FG har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/FG.png", width = 4, height = 4, bg="white")


V <- d %>%
  filter(Rolle == "medlem" | Rolle == "minister") %>%
  filter(Partibogstav == "V") %>%
  group_by(Emne, Partibogstav) %>%
  summarise(antal_min = round(sum(difftid)/60,2)) %>%
  arrange(desc(antal_min))


ggplot(V[1:5,], aes(x = antal_min, y= reorder(Emne, +antal_min), fill = Partibogstav))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
  theme_minimal()+ scale_fill_manual(values=partifarver)+
  geom_text(aes(label = round(antal_min, 1)), hjust = 1.3, size = 3, colour = "white", fontface = "bold")+
  labs(
    title = "FT-debatter som MF'er fra V har deltaget mest i
    (tal = minutter)",
    caption = "Note: I Mette F's regeringsperiode. Tal i kolonner 
    angiver antal minutter, som MF'er fra partiet har 
    haft ordet i den pågældende debat",
    x = element_blank(),
    y = element_blank()) + theme(plot.title = element_text(size=12, hjust = 1), legend.position = 'none', )

ggsave("C:/Users/malth/OneDrive/Kodning/Folketinget/Lovbehandlinger/V.png", width = 4, height = 4, bg="white")




