library(whisker)
template.file <- '1_spatial/in/1_user_catchments.yml.mustache'
template <- readChar(template.file , file.info(template.file )$size)
shp.relative.root <- '1_spatial/cache/POWSTREAMS_NAD83'
all.files <- dir(shp.relative.root)
library(dplyr) 
shp.layers <- data.frame(files = all.files, stringsAsFactors = FALSE) %>% 
  filter(grepl('.dbf', x = files)) %>% .$files %>% 
  sapply(function(x) strsplit(x, '[.]')[[1]][1], USE.NAMES = FALSE)


sites <- list()
for (id in shp.layers){
  dbf.file <- file.path(shp.relative.root, all.files[grepl(sprintf('%s.', id), x = all.files) & grepl('.dbf', all.files)])
  other.files <- file.path(shp.relative.root, all.files[grepl(sprintf('%s.', id), x = all.files) & !grepl('.dbf', all.files)])
  sites <- append(sites, list(list(id = id, dbf = dbf.file, depends = other.files, comma = TRUE, close = FALSE)))
}
sites[[length(sites)]]$comma = FALSE
sites[[length(sites)]]$close = TRUE

cat(whisker::whisker.render(template), file = 'remake/1_user_catchments.yml')
