library(tidyverse)
library(tbeptools)
library(here)

xlsfl <- here("R/data-raw/epcdat.xlsx")
epcdat <- read_importwq(xlsxf, download_latest = T)

subseg <- tibble(
  epchc_station = c(46, 64, 42, 65, 66), 
  subsegment = c("NW", "NW", "CW", "CW", "CW")
)

subdat <- epcdat |> 
  select(bay_segment, epchc_station, yr, Month = mo, chla) |> 
  inner_join(subseg, by = "epchc_station") |>
  filter(!is.na(subsegment)) |> 
  filter(yr %in% c((max(yr) - 5):max(yr))) |> 
  summarise(
    chla = mean(chla, na.rm = T), 
    .by = c(subsegment, yr, Month)
  )

hisdat <- subdat |> 
  filter(yr < (max(yr))) |> 
  summarise(
    chla = mean(chla, na.rm = T), 
    .by = c(subsegment, Month)
  ) |> 
  mutate(
    Month = factor(Month, levels = 1:12, labels = month.abb)
  ) |> 
  pivot_wider(names_from = subsegment, values_from = chla) |> 
  arrange(Month) |> 
  mutate(
    Month = as.character(Month),
  )

curdat <- subdat |> 
  filter(yr == max(yr)) |> 
  mutate(
    Month = factor(Month, levels = 1:12, labels = month.abb)
  ) |> 
  select(-yr) |> 
  pivot_wider(names_from = subsegment, values_from = chla, id_expand = T) |> 
  arrange(Month) |> 
  select(-Month) |> 
  as.list()

save(hisdat, file = here('data/hisdat.RData'))
save(curdat, file = here('data/curdat.RData'))

file.remove(xlsfl)