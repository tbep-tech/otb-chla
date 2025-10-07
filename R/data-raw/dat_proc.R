library(tidyverse)
library(tbeptools)
library(here)

source(here('R/funcs.R'))

# prep and save current and five year data for dash ----------------------

xlsfl <- here("R/data-raw/epcdat.xlsx")
epcdat <- read_importepc(xlsfl, download_latest = T)

subseg <- tibble(
  epchc_station = c('46', '64', '42', '65', '66'), 
  subsegment = c("NW", "NW", "CW", "CW", "CW")
)

epcchl <- epcdat |> 
  separate(YearMonth, c('yr', 'Month'), sep = '-', convert = T) |>
  select(epchc_station = StationNumber, yr, Month, chla = Chlorophyll_a) |> 
  inner_join(subseg, by = "epchc_station") |>
  filter(yr >= 2000) |> 
  mutate(chla = as.numeric(chla))

save(epcchl, file = here('data/epcchl.RData'))

file.remove(xlsfl)

# bootstrap prob of exceeding in current year ----------------------------

# currently an unweighted bootstrap
# need to weight years with similar chlorophyll in months with available data more heavily

load(here('data/epcchl.RData'))

thresholds <- tibble(
  subsegment = c('NW', 'CW'),
  thresh = c(11.3, 13.8)
)

mos <- 6:10

curyr <- epcchl |> 
  filter(Month %in% mos) |> 
  filter(yr == max(yr))

curmo <- curyr |> 
  pull(Month) |> 
  unique()

tosmp <- epcchl |> 
  filter(yr != max(yr)) |> 
  filter(Month %in% setdiff(mos, curmo)) 

nboot <- 10000
chk <- map_dfr(1:nboot, function(i) {
  tosmp |> 
    slice_sample(n = 1, replace = TRUE, by = c(Month, subsegment, epchc_station)) |> 
    mutate(bootstrap_id = i)
})

chksum <- chk |> 
  summarise(chla = mean(chla, na.rm = T), .by = c(Month, subsegment, bootstrap_id))

# est means
curyrmn <- curyr |> 
  summarise(chla = mean(chla, na.rm = T), .by = c(Month, subsegment)) |> 
  crossing(bootstrap_id = 1:nboot)

allests <- bind_rows(chksum, curyrmn) |> 
  summarise(chla = mean(chla, na.rm = T), .by = c(subsegment, bootstrap_id)) |> 
  inner_join(thresholds, by = "subsegment") |>
  mutate(exceed = chla > thresh)

prob <- allests |>
  summarise(prob = mean(exceed), .by = subsegment) |> 
  mutate(prob = round(100 * prob, 2))

save(prob, file = here('data/prob.RData'))
