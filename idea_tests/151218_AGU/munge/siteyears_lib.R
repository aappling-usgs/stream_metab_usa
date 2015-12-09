# Munge: create list of dfs of GPP & ER & K600 with 365 days per col, 1 col per site-year

library(dplyr)
library(tidyr)

make_siteyears_dfs <- function(preds, values=c('GPP','ER','K600')) {
  preds <- preds %>% mutate(
    doy=as.numeric(format(local.date, "%j")),
    year=as.numeric(format(local.date, "%Y")),
    siteyear=paste0(site,":",year))
  sylist <- lapply(values %>% setNames(.,.), function(v) {
    siteyears <- preds %>%
      select_('doy', 'siteyear', v) %>%
      spread_('siteyear', v) %>%
      arrange(doy) %>%
      right_join(data_frame(doy=1:365), by='doy') %>%
      select(-doy)
    emptycols <- which(sapply(siteyears, function(col) length(which(!is.na(col)))) == 0)
    if(length(emptycols)==0) siteyears else siteyears[-emptycols]
  })
}

make_growyears_dfs <- function(siteyears) {
  growdates <- do.call(seq, as.list(as.numeric(format(as.Date(c("2015-03-15", "2015-09-15")), "%j"))))
  lapply(siteyears, function(sy) sy[growdates,])
}

are_complete_siteyears <- function(siteyears, min_rows=c(year=300, grow=150), max_gap=21) {
  type <- if(dim(siteyears[[1]])[1] == 365) 'year' else 'grow'
  counts <- lapply(siteyears, function(sy) {
    sapply(sy, function(col) {
      rows <- which(!is.na(col))
      has_min_rows <- length(rows) >= min_rows[[type]]
      biggest_gap <- max(diff(c(0, rows, length(col)+1)))
      hasnt_max_gap <- biggest_gap <= max_gap
      has_min_rows && hasnt_max_gap
    })
  })
  if(!all.equal(counts[[1]], counts[[2]]) || !all.equal(counts[[1]], counts[[3]])) stop("unequal counts")
  counts[[1]]
}
