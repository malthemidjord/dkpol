require(tidyverse)
library(stringr)
library(jsonlite)

read_my_json <- function(x) {
  doc <- jsonlite::fromJSON(x)
  id <- doc[["value"]][["id"]]
  navn <-doc[["value"]][["navn"]]
  fornavn <- doc[["value"]][["fornavn"]]
  efternavn <- doc[["value"]][["efternavn"]]
  biografi <- doc[["value"]][["biografi"]]
  df <- data.frame(id, navn, fornavn, efternavn, biografi)}

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=0"
dat0 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=100"
dat1 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=200"
dat2 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=300"
dat3 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=400"
dat4 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=500"
dat5 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=600"
dat6 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=700"
dat7 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=800"
dat8 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=900"
dat9 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1000"
dat10 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1100"
dat11 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1200"
dat12 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1300"
dat13 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1400"
dat14 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1500"
dat15 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1600"
dat16 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1700"
dat17 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1800"
dat18 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=1900"
dat19 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2000"
dat20 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2100"
dat21 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2200"
dat22 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2300"
dat23 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2400"
dat24 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2500"
dat25 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2600"
dat26 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2700"
dat27 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2800"
dat28 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=2900"
dat29 <- map_df(json_file, read_my_json)

json_file <- "https://oda.ft.dk/api/Akt%C3%B8r?$inlinecount=allpages&$filter=typeid%20eq%205&$skip=3000"
dat30 <- map_df(json_file, read_my_json)

total <- rbind(dat0, dat1, dat2 , dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11,
               dat12, dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20, dat21, dat22,
               dat23, dat24, dat25, dat26, dat27, dat28, dat29, dat30)

total <- total[!(total$biografi == ""),]
total <- total[!(is.na(total$navn)),]

write.csv(total,"C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MF baggrund", row.names = FALSE)

total <- read.csv("C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MF baggrund")


total$status <- str_extract(total$biografi, pattern = "(?<=<status>)\\d+(?=</status>)")
total$profession <- str_extract(total$biografi, pattern = "(?<=<profession>)[\\w\\W\\s]+(?=</profession>)")
total$born <- str_extract(total$biografi, pattern = "(?<=<born>)[\\d\\W]+(?=</born>)")
total$uddannelsesniveau <- str_extract(total$biografi, pattern = "(?<=<educationStatistic>)\\w+(?=</educationStatistic>)")
total$ftstartdato <- str_extract(total$biografi, pattern = "(?<=<functionStartDate>)[\\w\\W\\s\\d]+(?=</functionStartDate>)")
total$kreds_nu <- str_extract(total$biografi, pattern = "(?<=<currentConstituency>)[\\w\\W\\s\\d]+(?=</currentConstituency>)")
total$kreds_tidligere <- str_extract(total$biografi, pattern = "(?<=<constituency>)[\\w\\W\\s\\d]+(?=</constituency>)")
total$Uddannelse <- str_extract(total$biografi, pattern = "(?<=<education>)[\\w\\W\\s\\d]+(?=</education>)")
total$erhvervserf <- str_extract(total$biografi, pattern = "(?<=<occupation>)[\\w\\W\\s\\d]+(?=</occupation>)")
total$køn <- str_extract(total$biografi, pattern = "(?<=<sex>)\\w+(?=</sex>)")
total$parti <- str_extract(total$biografi, pattern = "(?<=<party>)[\\w\\W\\s\\d]+(?=</party>)")
total$navn_parti <- str_extract(total$biografi, pattern = "(?<=<title>)[\\w\\W\\s\\d]+(?=</title>)")

write.csv(total,"C:/Users/malth/OneDrive/Kodning/Folketinget/Data/MF baggrund", row.names = FALSE)
