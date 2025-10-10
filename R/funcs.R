datsum <- function(epcchl){

  subdat <- epcchl |> 
    dplyr::filter(yr %in% c((max(yr) - 5):max(yr)))

  hisdat <- subdat |> 
    dplyr::filter(yr < (max(yr))) |> 
    dplyr::summarise(
      chla = mean(chla, na.rm = T), 
      .by = c(subsegment, Month)
    ) |> 
    dplyr::mutate(
      Month = factor(Month, levels = 1:12, labels = month.abb)
    ) |> 
    tidyr::pivot_wider(names_from = subsegment, values_from = chla) |> 
    dplyr::arrange(Month) |> 
    dplyr::mutate(
      Month = as.character(Month),
    )

  curdat <- subdat |> 
    dplyr::filter(yr == max(yr)) |> 
    dplyr::mutate(
      Month = factor(Month, levels = 1:12, labels = month.abb)
    ) |> 
    dplyr::select(-yr) |> 
    tidyr::pivot_wider(names_from = subsegment, values_from = chla, id_expand = T) |> 
    dplyr::arrange(Month) |> 
    dplyr::select(-Month) |> 
    as.list()

  out <- list(hisdat = hisdat, curdat = curdat)

  return(out)
  
}