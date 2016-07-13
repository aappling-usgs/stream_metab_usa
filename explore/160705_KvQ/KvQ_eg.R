kvqdata <- readRDS('KvQdata1.Rds')

kvqmodels <- lapply(setNames(nm=unique(kvqdata$site)), function(s) {
  onesite <- kvqdata[kvqdata$site==s, ]
  tryCatch(lm(K600 ~ log(disch), data=onesite), error=function(e) NULL)
})
r.squareds <- unlist(sapply(kvqmodels, function(kqm) {
  tryCatch(summary(kqm)$r.squared, error=function(e) NULL)
}))
