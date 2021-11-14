library(tidyverse)

#' Data prep for Datawrapper map
#' Map editor: https://app.datawrapper.de/map/b69Bt/data

url_btw <- "https://www.bundeswahlleiter.de/bundestagswahlen/2021/ergebnisse/opendata/csv/kerg2.csv"
btw <- read_csv2(url_btw, skip = 9)
colnames(btw) <- str_to_lower(colnames(btw))

btw_wbt <- btw %>%
  filter(wahlart == "BT", wahltag == "26092021", gebietsart == "Wahlkreis",
         gruppenart == "System-Gruppe",
         gruppenname == "Wählende") %>%
  mutate(wkr_nr = as.numeric(gebietsnummer)) %>%
  select(wkr_nr, wkr_name = "gebietsname", wbt = prozent)
  # select(wkr_name = "gebietsname", VALUE = prozent)

write_csv(btw_wbt, here::here("data", "bundestagswahl2021-wbt.csv"))
