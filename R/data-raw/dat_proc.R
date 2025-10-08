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

# get weights
maxmo <- epcchl |> 
  filter(yr == max(yr)) |> 
  pull(Month) |> 
  max()

chlsum <- epcchl |> 
  filter(Month <= maxmo) |> 
  summarise(chla = mean(chla, na.rm = T), .by = c(yr, Month, subsegment))

curyr <- chlsum |> 
  filter(yr == max(yr))

wts <- chlsum |> 
  filter(yr != max(yr)) |> 
  left_join(curyr, by = c("Month", "subsegment"), suffix = c("", "_cur")) |> 
  summarize(
    rmse = sqrt(mean((chla - chla_cur)^2, na.rm = T)),
    .by = c(yr, subsegment)
  ) |> 
  mutate(
    wt = exp(-rmse),
    wt = wt / sum(wt),
    .by = subsegment
  )

# toplo <- chlsum |> 
#   left_join(wts, by = c("yr", "subsegment")) 

# ggplot(data = toplo |> filter(yr != max(yr)), aes(x = Month, y = chla, group = yr, alpha = wt)) + 
#   geom_line() +
#   geom_line(data = toplo |> filter(yr == max(yr)), aes(x = Month, y = chla, color = 'Current year'), size = 1) +
#   scale_color_manual(values = c('Current year' = 'blue')) +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   theme_minimal(base_size = 14) + 
#   theme(
#     legend.position = 'bottom',
#     panel.grid.minor = element_blank() 
#   ) +
#   facet_wrap( ~ subsegment) + 
#   labs(
#     title = 'Monthly mean chlorophyll by OTB subsegment', 
#     subtitle = 'Weighted by similarity to current year in months with data', 
#     x = NULL, 
#     y = expression('Chlorophyll a ('*mu*'g/L)'),
#     color = NULL, 
#     alpha = 'Bootstrap weight'
#   )

# get probabilities

curyr <- epcchl |> 
  filter(Month %in% mos) |> 
  filter(yr == max(yr))

curmo <- curyr |> 
  pull(Month) |> 
  unique()

tosmp <- epcchl |> 
  filter(yr != max(yr)) |> 
  filter(Month %in% setdiff(mos, curmo)) |> 
  left_join(wts, by = c("yr", "subsegment"))

nboot <- 10000
chk <- map_dfr(1:nboot, function(i) {
  tosmp |> 
    slice_sample(n = 1, replace = TRUE, by = c(Month, subsegment, epchc_station), weight_by = wt) |> 
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


